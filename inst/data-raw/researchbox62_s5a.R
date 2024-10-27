setwd(this.path::here())
xforecast <- read.csv("data/researchbox62-s4_clean.csv", header = TRUE) |>
  dplyr::filter(check == 4,
                !is.na(extreme)) |>
  dplyr::mutate(id = factor(as.integer(factor(responseid))),
                stock = factor(stock),
                condition = factor(condition)) |>
  dplyr::select(extreme, answer, stock, id, condition)
usethis::use_data(xforecast)
