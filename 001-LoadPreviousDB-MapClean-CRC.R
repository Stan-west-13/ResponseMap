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


# Revise indices in RESPONSE_MAP ----
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


# Revise indices in RESPONSE_BEHAVIORS ----
tbls$response_behaviors[[3]] <- tbls$response_behaviors[[3]] |>
  separate_wider_delim(subject_id, delim = "_", names = c("study_code", "subject_id")) |>
  mutate(subject_id = as.integer(subject_id))

response_behaviors <- tbls$response_behaviors |>
  list_rbind(names_to = "study_id") |>
  select(
    study_id,
    old_id = id,
    subject_id,
    old_cue_id=cue_id,
    old_response_id=response_id,
    cue_order,
    response_order
  ) |>
  left_join(
    id_map_cues |> select(study_id, old_cue_id=old_id, cue_id=new_id),
    by = join_by(study_id, old_cue_id)
  ) |>
  left_join(
    id_map_resps |> select(study_id, old_response_id=old_id, response_id=new_id),
    by = join_by(study_id, old_response_id)
  ) |>
  mutate(id = seq_len(n())) |>
  select(id, study_id, subject_id, cue_id, response_id, cue_order, response_order)



# Revise mappings ----
resp_map_orig <- resp_map
resp_map <- resp_map_orig |>
  left_join(distinct_cues |> rename(cue_id=id)) |>
  left_join(distinct_resps |> rename(response_id=id)) |>
  mutate(
    revision = replace(revision, revision == "a ball", "ball"),
    subtlex_id = replace(subtlex_id, revision == "ball", 938),
    kuperman_id = replace(kuperman_id, revision == "ball", 3128)
  ) |>
  mutate(
    revision = replace(revision, revision == "a dad", "dad"),
    subtlex_id = replace(subtlex_id, revision == "dad", 471),
    kuperman_id = replace(kuperman_id, revision == "dad", 11352)
  ) |>
  mutate(
    revision = replace(revision, revision == "babies", "baby"),
    subtlex_id = replace(subtlex_id, revision == "baby", 391),
    kuperman_id = replace(kuperman_id, revision == "baby", 2910)
  ) |>
  mutate(
    revision = replace(revision, revision == "babmi", "bambi")
  ) |>
  mutate(
    revision = replace(revision, revision == "bed time", "bedtime"),
    subtlex_id = replace(subtlex_id, revision == "bedtime", 4884),
    kuperman_id = replace(kuperman_id, revision == "bedtime", 3716)
  ) |>
  mutate(
    revision = replace(revision, revision == "belly button", "bellybutton"),
    subtlex_id = replace(subtlex_id, revision == "bellybutton", 19267),
    kuperman_id = replace(kuperman_id, revision == "bellybutton", 3882)
  ) |>
  mutate(
    revision = replace(revision, revision == "boe boe", "boo boo"),
    revision = replace(revision, revision == "booboo", "boo boo"),
    revision = replace(revision, revision == "bow bow", "boo boo"),
    subtlex_id = replace(subtlex_id, revision == "boo boo", 50848)
  ) |>
  mutate(
    revision = replace(revision, revision == "broke something", "broke"),
    subtlex_id = replace(subtlex_id, revision == "broke", 646),
    kuperman_id = replace(kuperman_id, revision == "broke", 5577)
  ) |>
  mutate(
    revision = replace(revision, revision == "broken heart", "heart"),
    subtlex_id = replace(subtlex_id, revision == "heart", 450),
    kuperman_id = replace(kuperman_id, revision == "heart", 21340)
  ) |>
  mutate(
    revision = replace(revision, revision == "broken leg", "leg"),
    subtlex_id = replace(subtlex_id, revision == "leg", 1227),
    kuperman_id = replace(kuperman_id, revision == "leg", 26081)
  ) |>
  mutate(
    revision = replace(revision, revision == "broken toys", "toy"),
    subtlex_id = replace(subtlex_id, revision == "toy", 2886),
    kuperman_id = replace(kuperman_id, revision == "toy", 46977)
  ) |>
  mutate(
    revision = replace(revision, revision == "animals", "animal"),
    subtlex_id = replace(subtlex_id, revision == "animal", 1380),
    kuperman_id = replace(kuperman_id, revision == "animal", 1565)
  ) |>
  mutate(
    subtlex_id = replace(subtlex_id, revision == "appetizing", 19567),
    kuperman_id = replace(kuperman_id, revision == "appetizing", 1912)
  ) |>
  mutate(
    subtlex_id = replace(subtlex_id, revision == "applause", 4065),
    kuperman_id = replace(kuperman_id, revision == "applause", 1917)
  ) |>
  mutate(
    revision = replace(revision, revision == "are family", "family"),
    subtlex_id = replace(subtlex_id, revision == "family", 362),
    kuperman_id = replace(kuperman_id, revision == "family", 16852)
  ) |>
  mutate(
    revision = replace(revision, revision == "are going", "going"),
    subtlex_id = replace(subtlex_id, revision == "going", 80),
    kuperman_id = replace(kuperman_id, revision == "going", 19880)
  ) |>
  mutate(
    revision = replace(revision, revision == "are one", "one"),
    subtlex_id = replace(subtlex_id, revision == "one", 37),
    kuperman_id = replace(kuperman_id, revision == "one", 31114)
  ) |>
  mutate(
    revision = replace(revision, revision == "back aches", "ache"),
    subtlex_id = replace(subtlex_id, revision == "ache", 8825),
    kuperman_id = replace(kuperman_id, revision == "ache", 358)
  ) |>
  mutate(
    revision = replace(revision, revision == "back handspring", "handspring"),
    subtlex_id = replace(subtlex_id, revision == "handspring", 43411),
    kuperman_id = replace(kuperman_id, revision == "", 20972)
  ) |>
  mutate(
    revision = replace(revision, revision == "bacne", "acne"),
    subtlex_id = replace(subtlex_id, revision == "acne", 17229),
    kuperman_id = replace(kuperman_id, revision == "acne", 389)
  ) |>
  mutate(
    revision = replace(revision, revision == "bad news", "news"),
    subtlex_id = replace(subtlex_id, revision == "news", 513),
    kuperman_id = replace(kuperman_id, revision == "news", 30171)
  ) |>
  mutate(
    revision = replace(revision, revision == "bad smell", "smell"),
    subtlex_id = replace(subtlex_id, revision == "smell", 822),
    kuperman_id = replace(kuperman_id, revision == "smell", 42075)
  ) |>
  mutate(
    revision = replace(revision, revision == "bakkkk", "bawk")
  ) |>
  mutate(
    revision = replace(revision, revision == "bestfriend", "friend"),
    revision = replace(revision, revision == "bestie", "friend"),
    revision = replace(revision, revision == "bff", "friend"),
    subtlex_id = replace(subtlex_id, revision == "friend", 274),
    kuperman_id = replace(kuperman_id, revision == "friend", 18691)
  ) |>
  mutate(
    revision = replace(revision, revision == "brushing teeth", "brush teeth"),
    subtlex_id = replace(subtlex_id, revision == "brush teeth", 1198),
    kuperman_id = replace(kuperman_id, revision == "brush teeth", 45795)
  ) |>
  mutate(
    revision = replace(revision, revision == "bumper cars", "bumper car"),
    subtlex_id = replace(subtlex_id, revision == "bumper car", 381),
    kuperman_id = replace(kuperman_id, revision == "bumper car", 6602)
  ) |>
  mutate(
    revision = replace(revision, revision == "burpees", "burpee")
  ) |>
  mutate(
    revision = replace(revision, revision == "bye bye", "byebye")
  ) |>
  mutate(
    revision = replace(revision, revision == "cakadoodldoo", "cockadoodledoo"),
    revision = replace(revision, revision == "cock a doodle doo", "cockadoodledoo"),
    revision = replace(revision, revision == "cocka doodle doo", "cockadoodledoo"),
    revision = replace(revision, revision == "cockadoodle", "cockadoodledoo"),
    revision = replace(revision, revision == "cockadoodledoooooo", "cockadoodledoo"),
  ) |>
  mutate(
    revision = replace(revision, revision == "cold play", "coldplay")
  ) |>
  mutate(
    revision = replace(revision, revision == "carniveros", "carnivorous"),
    subtlex_id = replace(subtlex_id, revision == "carnivorous", 21568),
    kuperman_id = replace(kuperman_id, revision == "carnivorous", 6728)
  ) |>
  mutate(
    revision = replace(revision, revision == "cartilage", "cartilage"),
    subtlex_id = replace(subtlex_id, revision == "cartilage", 18961),
    kuperman_id = replace(kuperman_id, revision == "cartilage", 6789)
  ) |>
  mutate(
    revision = replace(revision, revision == "cat food", "cat"),
    subtlex_id = replace(subtlex_id, revision == "cat", 1371),
    kuperman_id = replace(kuperman_id, revision == "cat", 6891)
  ) |>
  mutate(
    revision = replace(revision, revision == "cell wall", "cell"),
    subtlex_id = replace(subtlex_id, revision == "cell", 1326),
    kuperman_id = replace(kuperman_id, revision == "cell", 7101)
  ) |>
  mutate(
    revision = replace(revision, revision == "chic fil a", "chick-fil-a"),
    revision = replace(revision, revision == "chick fil a", "chick-fil-a"),
    revision = replace(revision, revision == "chickfila", "chick-fil-a"),
    revision = replace(revision, revision == "chickfilla", "chick-fil-a"),
    revision = replace(revision, revision == "chickfla", "chick-fil-a")
  ) |>
  mutate(
    revision = replace(revision, revision == "collab", "collaborate"),
    revision = replace(revision, revision == "collaborative", "collaborate"),
    subtlex_id = replace(subtlex_id, revision == "collaborate", 20324),
    kuperman_id = replace(kuperman_id, revision == "", 8643)
  ) |>
  mutate(
    subtlex_id = replace(subtlex_id, revision == "comfy", 9155),
    kuperman_id = replace(kuperman_id, revision == "comfy", 8826)
  ) |>
  mutate(
    subtlex_id = replace(subtlex_id, revision == "comprehension", 14093),
    kuperman_id = replace(kuperman_id, revision == "comprehension", 9118)
  ) |>
  mutate(
    subtlex_id = replace(subtlex_id, revision == "congratulations", 873),
    kuperman_id = replace(kuperman_id, revision == "congratulations", 9433)
  ) |>
  mutate(
    revision = replace(revision, revision == "cant", "cannot"),
    subtlex_id = replace(subtlex_id, revision == "cannot", 663),
    kuperman_id = replace(kuperman_id, revision == "cannot", 6476)
  ) |>
  mutate(
    subtlex_id = replace(subtlex_id, revision == "delicious", 2081),
    kuperman_id = replace(kuperman_id, revision == "delicious", 12043)
  ) |>
  mutate(
    revision = replace(revision, revision == "could've", "could"),
    subtlex_id = replace(subtlex_id, revision == "could", 82),
    kuperman_id = replace(kuperman_id, revision == "could", 10227)
  ) |>
  mutate(
    revision = replace(revision, revision == "coy bow", "cowboy"),
    subtlex_id = replace(subtlex_id, revision == "cowboy", 3229),
    kuperman_id = replace(kuperman_id, revision == "cowboy", 10465)
  ) |>
  mutate(
    revision = replace(revision, revision == "cross body", "crossbody"),
  ) |>
  mutate(
    revision = replace(revision, revision == "cru", "cruse"),
  ) |>
  mutate(
    revision = replace(revision, revision == "delish", "delicious"),
    subtlex_id = replace(subtlex_id, revision == "delicious", 2081),
    kuperman_id = replace(kuperman_id, revision == "delicious", 12043)
  )
  

# An inconsistent mapping is any response ID with multiple different revisions,
# kuperman_id, or subtlex_id
inconsistent_mapping <- resp_map |>
  arrange(response_id) |>
  group_by(response_id) |>
  filter(if_any(c(revision, kuperman_id, subtlex_id), ~ n_distinct(.x) > 1))


# Load Stan's revisions ----
stan_rev <- readr::read_csv("data/cross_study_cleanedRevisions.csv")

# Merge Stan's revisions into the inconsistent mappings
xx <- inconsistent_mapping |>
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
  ungroup() |>
  select(cue, response, revision, subtlex_id, kuperman_id, starts_with("stan_"))

# Write out inconsistent mappings for further reconciliation
readr::write_csv(xx, "inconsistent-mappings.csv")

# Read back the reconciles mappings, without inconsistencies
revised_mapping <- readr::read_csv("inconsistent-mappings-reconciled.csv")

resp_map_revised <- resp_map |>
  select(
    id=new_id,
    study_id,
    cue_id,
    response_id,
    researcher_id,
    timestamp,
    cue, response
  ) |>
  left_join(
    revised_mapping |>
      select(
        cue,
        response,
        revision=new_revision,
        subtlex_id=new_subtlex_id,
        kuperman_id=new_kuperman_id
      ) |>
      distinct()
  ) |>
  mutate(revision = if_else(response == revision, NA, revision))

inconsistent_mapping_check <- resp_map_revised |>
  arrange(response_id) |>
  group_by(response_id) |>
  filter(if_any(c(revision, kuperman_id, subtlex_id), ~ n_distinct(.x) > 1))


saveRDS(resp_map_revised, "tables/rds/resp-map-revised.rds")
saveRDS(tbls$kuperman[[1]], "tables/rds/kuperman.rds")
saveRDS(tbls$subtlex[[1]], "tables/rds/subtlex.rds")

    
# Write tables ----
new_tbls <- list(
  cues = distinct_cues,
  cues_responses = cue_resp_map,
  decisions = tbls$decisions[[1]], # all the same
  kuperman = tbls$kuperman[[1]], # all the same
  subtlex = tbls$subtlex[[1]], # all the same
  researchers = tbls$researchers |> list_rbind() |> distinct() |> mutate(id = seq_len(n()))
)


tbls$response_behaviors[[1]]
