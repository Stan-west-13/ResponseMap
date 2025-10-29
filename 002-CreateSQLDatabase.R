library(stringr)
library(tidyr)
library(dplyr)
library(readr)
## Load current responses and response maps
d <- read.csv("data/Metadata_MM_2025-10-22.csv") %>%
  mutate(study_id = 4)

cleaned <- read.csv("data/cross_study_cleanedRevisions.csv")

cues_responses_archive <- read.csv("data/studyWise_cues_responses.csv")
## Prepare tables

## AoA Table
kuperman_table <- read_csv("data/AoA_51715_words.csv") %>%
  mutate(
    id = seq_len(n()),
    word = tolower(Word)
  ) %>%
  select(id, word, aoa = AoA_Kup_lem) %>%
  drop_na()

## Word Frequency Table
subtlex_table <- read_csv("data/SUBTLEXusfrequencyabove1.csv") %>%
  mutate(
    id = seq_len(n()),
    word = tolower(Word)
  ) %>%
  select(id, word, Lg10WF,Lg10CD) %>%
  drop_na()



## Cues table
cue_table <- cues_responses_archive %>%
  select(cue) %>%
  distinct() %>%
  rbind(data.frame(cue = unique(d$cue))) %>%
  arrange(cue) %>%
  unique() %>%
  mutate(id = seq_len(n())) %>%
  select(id, cue)

## Cues-study table
cue_study_table <- cues_responses_archive %>%
  select(cue,study_id) %>%
  rbind(data.frame(study_id = 4, cue = unique(d$cue))) %>%
  unique() %>%
  left_join(cue_table, by = "cue") %>%
  rename(cue_id = id) %>%
  mutate(id = seq.int(n()),.before = study_id)



## Response by participant table 
response_behavior_table <- d %>%
  select(
    participant,
    cue,
    response_order,
    response,
    study_id
  ) %>%
  rbind(cues_responses_archive %>%
          select(participant = subject_id,
                 cue,
                 response_order,
                 response,
                 study_id)) %>%
  mutate(
    id = 1:nrow(.),.before = participant,
    response = tolower(str_trim(response))
  ) %>%
  left_join(
    cue_study_table %>% select(cue_study_id = id,cue,study_id),
    by = c("cue","study_id")
  ) %>%
  select(-cue)


## Table of unique responses and indexes
responses_table <- response_behavior_table %>%
  filter(!response == "") %>%
  select(response) %>%
  distinct() %>%
  arrange(response) %>%
  mutate(id = seq_len(n()),.before = response) 

## Table of study ids and response_ids 
response_study_table <- response_behavior_table %>%
  select(id,response,study_id) %>%
  left_join(responses_table %>% rename(response_id = id), by = "response") %>%
  select(-response)

response_behavior_table <- response_behavior_table %>%
  left_join(select(responses_table,response_id = id,response), by = "response") 




cues_responses_table <- response_behavior_table %>%
  select(cue_study_id, response_id) %>%
  distinct() %>%
  arrange(cue_study_id, response_id) %>%
  left_join(select(response_study_table,response_study_id = id,response_id),by="response_id") %>%
  mutate(id = seq_len(n()),.before = cue_study_id) %>%
  select(id, cue_study_id, response_study_id)


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
  id = 1:4,
  study = c("Adult Associations 1",
            "Adult Associations 2",
            "Word Association RT",
            "Melodies and Meanings")
)

study_responses_table <- data.frame(
  study_id = cues_responses_archive$study_id,
  response = cues_responses_archive$response
) %>%
  mutate(id = seq.int(n()),.before = study_id)
  

subjects_table <- d %>%
  select(
    participant,
    condition
  ) %>%
  unique()
subjects_table <- subjects_table[!subjects_table$id == 75128 & subjects_table$COND_ID == 2,]

words_meta_table <- kuperman_table %>%
  select(word, kuperman_id = id) %>%
  full_join(subtlex_table %>% select(word, subtlex_id = id), by = "word") %>%
  full_join(cue_table %>% select(word = cue, cue_id = id), by = "word")



