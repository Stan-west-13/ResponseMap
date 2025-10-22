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



