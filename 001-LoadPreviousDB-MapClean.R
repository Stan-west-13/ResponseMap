library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(tidyr)
source("R/dbInsertHelpers.R")
source("R/dbSelectHelpers.R")



## Collection 1 db connection
first_collection <- dbConnect(RSQLite::SQLite(), "semantic_association_validation-crc.db")

response_maps <- dbGetQuery(first_collection, 'SELECT * FROM response_map') %>%
  mutate(cue_response_id = as.integer(cue_response_id))
cues <- dbGetQuery(first_collection, 'SELECT * FROM cues')
responses <- dbGetQuery(first_collection, 'SELECT * FROM responses')
cue_response <- dbGetQuery(first_collection, 'SELECT * FROM cues_responses')
dbDisconnect(first_collection)

cues_response_maps1 <- cue_response %>%
  left_join(responses, by = c("response_id" = "id" )) %>%
  left_join(cues, by = c("cue_id" = "id")) %>%
  left_join(select(response_maps,cue_response_id,revision,subtlex_id,kuperman_id), 
            by = c("id" = "cue_response_id"))


## Collection 2 db connection
second_collection <- dbConnect(RSQLite::SQLite(), "semantic_association_validation-crc_2.db")

response_maps2 <- dbGetQuery(second_collection, 'SELECT * FROM response_map')%>%
  mutate(cue_response_id = as.integer(cue_response_id))
cues2 <- dbGetQuery(second_collection, 'SELECT * FROM cues')
responses2 <- dbGetQuery(second_collection, 'SELECT * FROM responses')
cue_response2 <- dbGetQuery(second_collection, 'SELECT * FROM cues_responses')
dbDisconnect(second_collection)

cues_response_maps2 <- cue_response2 %>%
  left_join(responses2, by = c("response_id" = "id" )) %>%
  left_join(cues2, by = c("cue_id" = "id")) %>%
  left_join(select(response_maps2,cue_response_id,revision,subtlex_id,kuperman_id), 
            by = c("id" = "cue_response_id"))

## Collection 3 db connection
third_collection <- dbConnect(RSQLite::SQLite(), "Word-AssociationRT.db")

response_maps3 <- dbGetQuery(third_collection, 'SELECT * FROM response_map')%>%
  mutate(cue_response_id = as.integer(cue_response_id))
cues3 <- dbGetQuery(third_collection, 'SELECT * FROM cues')
responses3 <- dbGetQuery(third_collection, 'SELECT * FROM responses')
cue_response3 <- dbGetQuery(third_collection, 'SELECT * FROM cues_responses')
dbDisconnect(third_collection)


cues_response_maps3 <- cue_response3 %>%
  left_join(responses3, by = c("response_id" = "id" )) %>%
  left_join(cues3, by = c("cue_id" = "id")) %>%
  left_join(select(response_maps3,cue_response_id,revision,subtlex_id,kuperman_id), 
            by = c("id" = "cue_response_id"))




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
