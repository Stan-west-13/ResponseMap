library(stringr)
library(tidyr)
library(dplyr)
library(readr)
## Load current responses and response maps
d <- read.csv("data/Metadata_MM_2025-10-22.csv")

cleaned <- read.csv("data/cross_study_cleanedRevisions.csv")

## Prepare tables

kuperman_table <- read_csv("data/AoA_51715_words.csv") %>%
  mutate(
    id = seq_len(n()),
    word = tolower(Word)
  ) %>%
  select(id, word, aoa = AoA_Kup_lem) %>%
  drop_na()

subtlex_table <- read_csv("data/SUBTLEXusfrequencyabove1.csv") %>%
  mutate(
    id = seq_len(n()),
    word = tolower(Word)
  ) %>%
  select(id, word, Lg10WF,Lg10CD) %>%
  drop_na()

cue_table <- d %>%
  select(cue) %>%
  distinct() %>%
  arrange(cue) %>%
  mutate(id = seq_len(n())) %>%
  select(id, cue)

response_behavior_table <- d %>%
  select(
    participant,
    cue,
    response_order,
    response
  ) %>%
  mutate(
    id = 1:nrow(.),.before = participant,
    response = tolower(str_trim(response))
  ) %>%
  left_join(
    cue_table %>% select(cue, cue_id = id),
    by = "cue"
  ) %>%
  select(-cue)

responses_table <- response_behavior_table %>%
  filter(!response == "") %>%
  select(response) %>%
  distinct() %>%
  arrange(response) %>%
  mutate(id = seq_len(n()),.before = response) 

response_behavior_table <- response_behavior_table %>%
  left_join(responses_table %>% rename(response_id = id), by = "response") %>%
  select(id, participant, cue_id, response_order, response_id, -response)

cues_responses_table <- response_behavior_table %>%
  select(cue_id, response_id) %>%
  distinct() %>%
  arrange(cue_id, response_id) %>%
  mutate(id = seq_len(n())) %>%
  select(id, cue_id, response_id)


response_map_table <- cues_responses_table %>%
  left_join(
    responses_table %>% rename(response_id = id),
    by = "response_id"
  ) %>%
  left_join(
    kuperman_table %>% select(response = word, kuperman_id = id),
    by = "response"
  ) %>%
  left_join(
    subtlex_table %>% select(response = word, subtlex_id = id),
    by = "response"
  ) %>%
  select(-cue_id) %>%
  left_join(
    cue_table %>% select(cue_id = id, response = cue),
    by = "response"
  ) %>%
  filter(!(is.na(kuperman_id) & is.na(subtlex_id))) %>%
  rename(cue_response_id = id) %>%
  mutate(revision = NA, researcher_id = NA, timestamp = NA, id = seq_len(n())) %>%
  select(id, cue_response_id, kuperman_id, subtlex_id, cue_id, revision, researcher_id, timestamp, -response)


study_table <- data.frame(
  id = 1:5,
  study = c("Adult Associations 1",
            "Adult Associations 2",
            "Word Association RT",
            "Melodies and Meanings",
            "Word Association RTWM")
)

