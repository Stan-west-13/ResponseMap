library(DBI)
library(RSQLite)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
source("R/dbInsertHelpers.R")
source("R/dbSelectHelpers.R")

# Database list ---
db_files <- c(
  "semantic_association_validation-crc.db",
  "semantic_association_validation-crc_2.db",
  "Word-AssociationRT.db"
)


# Load from database ---
db_connections <- map(db_files, ~ dbConnect(RSQLite::SQLite(), .x))

cue_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM cues') |>
      as_tibble()
})

resp_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM responses') |>
      as_tibble()
})

cue_resp_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM cues_responses') |>
      as_tibble()
})

resp_map_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM response_map') |>
      as_tibble() |>
      mutate(cue_response_id = as.integer(cue_response_id))
})

kuperman_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM kuperman') |>
      as_tibble()
})

subtlex_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM subtlex') |>
      as_tibble()
})

meta_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM words_meta') |>
      as_tibble()
})

decisions_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM decisions') |>
      as_tibble()
})

subject_decisions_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM subject_decisions') |>
      as_tibble()
})

subjects_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM subjects') |>
      as_tibble()
})

researchers_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM researchers') |>
      as_tibble()
})

behavior_tbls <- map(db_connections, \(db) {
    dbGetQuery(db, 'SELECT * FROM response_behaviors') |>
      as_tibble()
})

walk(db_connections, dbDisconnect)


## Reconcile CUE indices across databases ---
all_cues <- list_rbind(cue_tbls, names_to = "study_id") |>
  rename(old_id = id)

distinct_cues <- list_rbind(cue_tbls) |>
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
all_resps <- list_rbind(resp_tbls, names_to = "study_id") |>
  rename(old_id = id)

distinct_resps <- list_rbind(resp_tbls) |>
  distinct(response) |>
  arrange(response) |>
  mutate(id = seq_len(n())) |>
  relocate(id)

id_map_resps <- left_join(all_resps, rename(distinct_resps, new_id = id)) |>
  relocate(new_id, .after = old_id)


## Revise indices in CUE_RESPONSE ---
cue_resp_map <- cue_resp_tbls |>
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
  

## Revise indices in RESPONSE_MAP ---
resp_map <- resp_map_tbls |>
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
    by = c("study_id", "old_id")
  ) |>
  mutate(new_id = seq_len(n())) |>
  relocate(new_id, old_id, study_id, cue_id, response_id)
  

# Sanity Check ---
tmp <- map2(resp_map_tbls, cue_resp_tbls, ~ {
  left_join(
    select(.x, -cue_id),
    rename(.y, cue_response_id = id),
    by = "cue_response_id"
  )
}) |>
    map2(cue_tbls, ~ left_join(.x, rename(.y, cue_id = id), by = "cue_id")) |>
    map2(resp_tbls, ~ left_join(.x, rename(.y, response_id = id), by = "response_id")) |>
    map(~ relocate(.x, cue_id, response_id, cue, response, kuperman_id, subtlex_id) |> arrange(response))

### Clean dupes
cleaned <- read.csv("clean_dups.csv") %>%
  select(-n_revision)
responsemaps_all <- unique(rbind(cues_response_maps1,cues_response_maps2,cues_response_maps3)) %>%
  mutate(revision = trimws(revision),
         response = trimws(response),
         rowid = seq(1,nrow(.),1)) 

x <- responsemaps_all %>%
  filter(!is.na(revision),
         duplicated(response)) %>%
  group_by(response) %>%
  mutate(n_revision = length(unique(revision))) %>%
  arrange(desc(n_revision), revision) %>%
  unique() %>%
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
