d <- read.csv("data/Metadata_MM_2025-10-22.csv")

cleaned <- read.csv("data/cross_study_cleanedRevisions.csv")


d_revision <- d %>%
  left_join(unique(select(cleaned, response, revision, subtlex_id,kuperman_id)), by = "response")

p <- d_revision %>% group_by(participant,cue) %>% summarize(x = length(response),
                                                            response = response) %>%
  arrange(desc(x),response)
