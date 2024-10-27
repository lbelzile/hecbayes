setwd(this.path::here())
library(dplyr)
exchangerate <- read.csv(file = "data/amCharts.csv", header = TRUE) |>
   select(date, s1) |>
   filter(!is.na(s1)) |>
   mutate(date = lubridate::ymd(as.Date(lubridate::ymd_hms(date)))) |>
   rename(rate = s1)
usethis::use_data(exchangerate, overwrite = TRUE)