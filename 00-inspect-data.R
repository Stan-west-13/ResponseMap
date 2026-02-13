library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)


# Create list of all CSV files in data directory ----
file_list <- list.files("Data", pattern = "*.csv", full.names = TRUE, recursive = TRUE)[c(-1, -2, -3, -101, -102)]

# Read each file and parse into first response and all responses data frames ----

## First, define a function that reads one file at a time and processes it
read_exp_data <- function(filename) {
  x <- read_csv(filename) |>
    mutate(
      condition = factor(condition, levels = c("n", "cl", "c"), labels = c("noise", "classical", "child")),
      participant = as.factor(participant)
    )
  df_all_responses <- x |>
    filter(!is.na(trials.thisTrialN)) |>
    select(
      participant,
      condition,
      trial = trials.thisTrialN,
      cue,
      cue_id = trials.thisIndex,
      response_order,
      response,
      response_start = response.start,
      response_stop = response.stop
    )
  df_first_responses <- x |>
    filter(
      !is.na(trials.thisTrialN),
      response_order == 1
    ) |>
    select(
      participant,
      condition,
      trial = trials.thisTrialN,
      cue,
      cue_id = trials.thisIndex,
      response_order,
      response,
      rt = key_resp_cue.rt,
      response_start = response.start,
      response_stop = response.stop
    )
  return(list(first_response = df_first_responses, all_responses = df_all_responses))
} 

## Then, apply that function to each file using `map()`
d <- map(file_list, read_exp_data)


# Bind participant data frames ----

## `d` is a list of participants, each containing a list of two data frames. 
## We want to map over participants and extract one data frame at a time.
## Then we can bind the instances of the same data frame for all participants
## together into one data frame.
d_first_response <- map(d, ~ .x$first_response) |> list_rbind()
d_all_responses <- map(d, ~ .x$all_responses) |> list_rbind()

# merge in mappings
mappings <- readRDS("tables/rds/resp-map-revised.rds")
kuperman <- readRDS("tables/rds/kuperman.rds")
subtlex <- readRDS("tables/rds/subtlex.rds")

mappings |> filter(if_any(c(kuperman_id, subtlex_id), ~ is.na(.x)))

mappings <- readRDS("tables/rds/resp-map-revised.rds") |>
  select(response, )


# Plot summary of response times by condition ----
# Error bars are standard error for a 1-sample t-test against zero
d_first_response |>
  group_by(condition, participant) |>
  summarize(rt_mean_pp = mean(rt)) |>
  group_by(condition) |>
  summarize(
    rt_mean = mean(rt_mean_pp),
    rt_sd = sd(rt_mean_pp),
    n = n(),
    n_pp = n_distinct(participant),
    rt_se = rt_sd / sqrt(n_pp)
  ) |>
  ggplot(aes(x = condition, y = rt_mean)) +
    geom_bar(stat  = "identity", position = position_dodge()) +
    geom_errorbar(aes(
      ymin = rt_mean - rt_se,
      ymax = rt_mean + rt_se
    ))
  

## In this figure, each participant is a dot and the range reflects the standard
## deviation of their own response times.
d_first_response |>
  group_by(condition, participant) |>
  summarize(
    rt_mean_pp = mean(rt),
    rt_sd_pp = sd(rt)
  ) |>
  group_by(condition) |>
  mutate(pp_ind = seq_along(participant)) |>
  ggplot(aes(x = pp_ind, y = rt_mean_pp)) +
    geom_pointrange(aes(
      ymin = rt_mean_pp - rt_sd_pp,
      ymax = rt_mean_pp + rt_sd_pp
    )) +
  facet_wrap(vars(condition))
  
d_first_response |>
  ggplot(aes(x = participant, y = rt)) +
    geom_boxplot() +
    facet_wrap(vars(condition))

# Test for effect of condition on RT ---
m <- lmer(rt ~ condition + (1 | participant), data = d_first_response)
summary(m)
anova(m)
