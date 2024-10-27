upworthy_sesame <- tibble::tibble(
  headline = factor(paste0("H", 1:4)),
  impressions = c(3060L, 2982L, 3112L, 3083L),
  clicks = c(49L, 20L, 31L, 9L))
usethis::use_data(upworthy_sesame, overwrite = TRUE)
