library(bang)
data(rat, package = "bang")
rats <- data.frame(rat[1:70,])
colnames(rats) <- c("counts","n")
usethis::use_data(rats, version = 2, overwrite = TRUE)

data(temp2, package = "bang")
data(temp1, package = "bang")
# aov(index ~ RCP, data = temp2) |> emmeans::emmeans(specs = "RCP")
# aov(index ~ RCP, data = temp1) |> emmeans::emmeans(specs = "RCP")

library(dplyr)
climatechange <- temp2 |>
  mutate(temp = round(index,6),
         RCP = as.numeric(substr(RCP, start = 4, stop = 5))/10,
         GCM  = factor(stringr::str_extract(temp2$GCM, "[^-]+"))) |>
  select(temp, GCM, RCP)
usethis::use_data(climatechange, version = 2, overwrite = TRUE)

