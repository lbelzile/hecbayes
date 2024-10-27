setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
nonvoter <- read.csv("data/nonvoters_data.csv",
                     header = TRUE,
                     stringsAsFactors = TRUE)
voting <- nonvoter |>
  as_tibble() |>
  select(Q30,
         ppage,
         race,
         gender,
         income_cat,
         voter_category,
         educ,
         weight) |>
  rename(age = ppage,
         vote = voter_category,
         income = income_cat) |>
  mutate(
    race = factor(tolower(race)),
    gender = factor(tolower(gender)),
    income = factor(tolower(income)),
    vote = ordered(forcats::fct_relevel(
      factor(tolower(vote), c("rarely/never","sporadic","always") ))),
    educ = factor(tolower(educ)),
    party = factor(
    Q30,
    levels = as.character(c(-1,1,2,3,4,5)),
    labels = c("other","GOP","Dem",rep("other",3))),
    age = as.integer(age),
    weight = as.numeric(weight)) |>
  select(!Q30)
usethis::use_data(voting, overwrite = TRUE)


