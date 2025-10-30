library(DBI)
library(RSQLite)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
source("R/dbInsertHelpers.R")
source("R/dbSelectHelpers.R")
source("R/read_all_tables.R")


# Database list ---
db_files <- c(
  "semantic_association_validation-crc.db",
  "semantic_association_validation-crc_2.db",
  "Word-AssociationRT.db"
)


# Load from database ---
tbls <- read_all_tables(db_files)


# Reconcile CUE indices across databases ---
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


## Reconcile RESPONSE indices across databases ---
all_resps <- list_rbind(tbls$responses, names_to = "study_id") |>
  rename(old_id = id)

distinct_resps <- list_rbind(tbls$responses) |>
  distinct(response) |>
  arrange(response) |>
  mutate(id = seq_len(n())) |>
  relocate(id)

id_map_resps <- left_join(all_resps, rename(distinct_resps, new_id = id)) |>
  relocate(new_id, .after = old_id)


## Revise indices in CUE_RESPONSE ---
cue_resp_map <- tbls$cues_responses |>
  list_rbind(names_to = "study_id") |>
  select(study_id, old_id = id, old_cue_id = cue_id, old_resp_id = response_id) |>
  left_join(
    select(id_map_cues, study_id, old_cue_id = old_id, cue_id = new_id),
    by = c("study_id", "old_cue_id")
  ) |>
  left_join(
    select(id_map_resps, study_id, old_resp_id = old_id, response_id = new_id),
    by = c("study_id", "old_resp_id")
  ) |>
  mutate(new_id = seq_len(n())) |>
  relocate(new_id, study_id, cue_id, response_id)

tmp <- cue_resp_map |>
  group_by(study_id) |>
  group_split() |>
  map2(tbls$cues, ~ left_join(.x, rename(.y, old_cue_id=id, old_cue=cue), by = join_by(old_cue_id))) |>
  map2(tbls$responses, ~ left_join(.x, rename(.y, old_resp_id=id, old_resp=response), by = join_by(old_resp_id))) |>
  list_rbind() |>
  left_join(rename(distinct_resps, response_id = id), by = join_by(response_id))

tmp |> mutate(x = old_resp == response) |>  pull(x) |> all() 


## Revise indices in RESPONSE_MAP ---
resp_map <- tbls$response_map |>
  list_rbind(names_to = "study_id") |>
  select(
    study_id,
    old_id = id,
    kuperman_id,
    subtlex_id,
    revision,
    researcher_id,
    timestamp
  ) |>
  left_join(
    select(cue_resp_map, study_id, old_id, cue_id, response_id),
    by = join_by(study_id, old_id)
  ) |>
  mutate(new_id = seq_len(n())) |>
  relocate(new_id, old_id, study_id, cue_id, response_id)
  

# Sanity Check ---
tmp <- map2(tbls$response_map, tbls$cues_responses, ~ {
  left_join(
    select(.x, -cue_id),
    rename(.y, cue_response_id = id),
    by = "cue_response_id"
  )
}) |>
    map2(tbls$cues, ~ left_join(.x, rename(.y, cue_id = id), by = "cue_id")) |>
    map2(tbls$responses, ~ left_join(.x, rename(.y, response_id = id), by = "response_id")) |>
    map(~ {
      relocate(.x, cue_id, response_id, cue, response, kuperman_id, subtlex_id) |>
        arrange(response)
    })


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
