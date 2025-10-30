library(DBI)
library(RSQLite)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
source("R/dbInsertHelpers.R")
source("R/dbSelectHelpers.R")
source("R/read_all_tables.R")


# Database list ----
db_files <- c(
  "semantic_association_validation-crc.db",
  "semantic_association_validation-crc_2.db",
  "Word-AssociationRT.db"
)


# Load from database ----
tbls <- read_all_tables(db_files)


# Reconcile CUE indices across databases ----
all_cues <- list_rbind(tbls$cues, names_to = "study_id") |>
  rename(old_id = id)

distinct_cues <- list_rbind(tbls$cues) |>
  distinct(cue) |>
  arrange(cue) |>
  mutate(id = seq_len(n())) |>
  relocate(id)

study_cue_map <- left_join(
    all_cues,
    distinct_cues,
    by = c("cue")
  ) |>
  select(study_id, cue_id = id)

id_map_cues <- left_join(
    all_cues,
    rename(distinct_cues, new_id = id),
    by = join_by(cue)
  ) |>
  relocate(new_id, .after = old_id)


# Reconcile RESPONSE indices across databases ----
all_resps <- list_rbind(tbls$responses, names_to = "study_id") |>
  rename(old_id = id)

distinct_resps <- list_rbind(tbls$responses) |>
  distinct(response) |>
  arrange(response) |>
  mutate(id = seq_len(n())) |>
  relocate(id)

id_map_resps <- left_join(all_resps, rename(distinct_resps, new_id = id)) |>
  relocate(new_id, .after = old_id)


# Revise indices in CUES_RESPONSES ----
cue_resp_map <- tbls$cues_responses |>
  list_rbind(names_to = "study_id") |>
  select(
    study_id,
    old_id = id,
    old_cue_id = cue_id,
    old_resp_id = response_id
  ) |>
  left_join(
    id_map_cues |> select(
      study_id,
      old_cue_id = old_id,
      cue_id = new_id
    ),
    by = join_by(study_id, old_cue_id)
  ) |>
  left_join(
    id_map_resps |> select(
      study_id,
      old_resp_id = old_id,
      response_id = new_id
    ),
    by = join_by(study_id, old_resp_id)
  ) |>
  mutate(new_id = seq_len(n())) |>
  relocate(new_id, study_id, cue_id, response_id)


## Sanity check: cues_responses revision ----
# Map new and old response IDs to corresponding strings and check for equality
tmp <- cue_resp_map |>
  group_by(study_id) |>
  group_split() |>
  map2(tbls$cues, function(resp_map, cues) {
    left_join(
      resp_map,
      cues |> rename(old_cue_id=id, old_cue=cue),
      by = join_by(old_cue_id)
    )
  }) |>
  map2(tbls$responses, function(resp_map, responses) {
    left_join(
      resp_map,
      responses |> rename(old_resp_id=id, old_resp=response),
      by = join_by(old_resp_id)
    )
  }) |>
  list_rbind() |>
  left_join(
    distinct_resps |> rename(response_id=id),
    by = join_by(response_id)
  )

tmp |> mutate(x = old_resp == response) |>  pull(x) |> all() 

map(tbls$responses, ~.x |> filter(response == "toyota"))

## Revise indices in RESPONSE_MAP ---
resp_map <- tbls$response_map |>
  list_rbind(names_to = "study_id") |>
  select(
    study_id,
    old_id = cue_response_id,
    kuperman_id,
    subtlex_id,
    revision,
    researcher_id,
    timestamp
  ) |>
  left_join(
    cue_resp_map |> select(study_id, old_id, cue_id, response_id),
    by = join_by(study_id, old_id)
  ) |>
  mutate(new_id = seq_len(n())) |>
  relocate(new_id, old_id, study_id, cue_id, response_id)

inconsistent_mapping <- resp_map |>
  arrange(response_id) |>
  group_by(response_id) |>
  filter(if_any(c(revision, kuperman_id, subtlex_id), ~ n_distinct(.x) > 1))

# Load Stan's revisions ----
stan_rev <- readr::read_csv("data/cross_study_cleanedRevisions.csv")
inconsistent_mapping |>
  left_join(distinct_cues |> rename(cue_id=id)) |>
  left_join(distinct_resps |> rename(response_id=id)) |>
  left_join(
    stan_rev |>
      select(
        response,
        cue,
        stan_revision = revision,
        stan_subtlex_id = subtlex_id,
        stan_kuperman_id = kuperman_id
      ) |>
      distinct()
    ) |>
  select(study_id:revision, cue:stan_kuperman_id) |>
  filter(cue == "daddy")

cue_resp_words <- cue_resp_map |>
  left_join(distinct_cues |> rename(cue_id=id)) |>
  left_join(distinct_resps |> rename(response_id=id))

cue_resp_words |>
  filter(response == "toyota")

### Clean dupes
cleaned <- read.csv("clean_dups.csv") %>%
  select(-n_revision)

x <- resp_map |>
  filter(!is.na(revision), duplicated(response_id)) |>
  group_by(response_id) |>
  mutate(n_revision = length(unique(revision))) |>
  arrange(desc(n_revision), revision) |>
  unique() |>
  filter(n_revision > 1)

cleaned_maps <- rbind(responsemaps_all[!responsemaps_all$rowid %in% x$rowid,-9],cleaned) %>%
  mutate(rowid = seq(1,nrow(.),1))

z <- cleaned_maps %>% 
  select(response,revision) %>% 
  filter(!is.na(revision)) %>% 
  unique() %>% 
  group_by(response) %>% 
  summarize(n_revision = length(unique(revision)))


y <- cleaned_maps %>%
  mutate(rowid = seq(1,nrow(.),1)) %>%
  filter(!is.na(revision),!revision == "") %>%
  group_by(response) %>%
  mutate(n_revision = length(unique(revision))) %>%
  arrange(desc(n_revision), revision) %>%
  unique() %>%
  filter(n_revision > 1) %>%
  arrange(response)

cleaned2 <- read.csv("cleaned_2.csv") %>%
  select(-n_revision)

cleaned_maps <- rbind(cleaned_maps[!cleaned_maps$rowid %in% y$rowid,-9],cleaned2[,-9]) 

write.csv(cleaned_maps, "data/cross_study_cleanedRevisions.csv")
