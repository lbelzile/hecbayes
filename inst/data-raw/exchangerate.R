setwd(this.path::here())
library(dplyr)
# exchangerate <- read.csv(file = "data/amCharts.csv", header = TRUE) |>
#    select(date, s1) |>
#    filter(!is.na(s1)) |>
#    mutate(date = lubridate::ymd(as.Date(lubridate::ymd_hms(date)))) |>
#    rename(rate = s1)
# usethis::use_data(exchangerate, overwrite = TRUE)


exchangerate <- read.csv(file = "data/DEXUSEU.csv", header = TRUE) |>
  rename(date = observation_date, dexrate = DEXUSEU) |>
  filter(!is.na(dexrate)) |>
  mutate(date = lubridate::ymd(as.Date(lubridate::ymd(date))))
usethis::use_data(exchangerate, overwrite = TRUE)

y <- diff(log(exchangerate$dexrate))
