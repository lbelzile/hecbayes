library(tidyverse)
upworthy <- read_csv("https://osf.io/vy8mj/download")
#upworthy <- read_csv("inst/data-raw/upworthy.csv")
upworthy_restricted <-
  upworthy |>
  select(
    created_at, clickability_test_id, headline, impressions, clicks
  )
upworthy_restricted <-
  upworthy_restricted |>
  mutate(
    asks_question =
      str_detect(string = headline, pattern = "\\?")
  )

upworthy_question <- upworthy_restricted |>
  group_by(clickability_test_id, asks_question) |>
  summarize(impressions = sum(impressions),
            clicks = sum(clicks)) |>
  ungroup() |>
  rename(question = asks_question,
         id = clickability_test_id) |>
  # select(question, impressions, clicks) |>
  mutate(question = factor(question, levels = c(TRUE, FALSE), labels = c('yes', 'no')),
         id = as.integer(factor(id))) |>
  filter(duplicated(id, fromLast = TRUE) | duplicated(id)) |>
  mutate(id = factor(as.integer(factor(id))),
         impressions = as.integer(impressions),
         clicks = as.integer(clicks))
# Select only questions with duplicate IDs (so there are yes/no)

usethis::use_data(upworthy_question, overwrite = TRUE)
sinew::makeOxygen(upworthy_question)

# upworthy_restricted |>
#   count(asks_question)
# question_or_not <-
#   upworthy_restricted |>
#   summarise(
#     ave_clicks = mean(clicks/impressions),
#     .by = c(clickability_test_id, asks_question)
#   )
# question_or_not |>
#   pivot_wider(names_from = asks_question,
#               values_from = ave_clicks,
#               names_prefix = "ave_clicks_") |>
#   drop_na(ave_clicks_FALSE, ave_clicks_TRUE) |>
#   mutate(difference_in_clicks = ave_clicks_TRUE - ave_clicks_FALSE) |>
#   summarise(average_difference = mean(difference_in_clicks))
