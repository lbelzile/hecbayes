setwd(this.path::here())

uscitypopn <- read.csv(file = "data/popn_size.csv", header = TRUE)
colnames(uscitypopn) <- tolower(colnames(uscitypopn))
usethis::use_data(uscitypopn, overwrite = TRUE)
