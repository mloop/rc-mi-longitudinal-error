library(tidyverse)

files <- dir(path = "../output/", pattern = "05_02")

imps <- paste0("../output/", files, sep = "") %>%
  map_dfr(read_rds)

write_rds(imps, "../output/05_multiple_imputation.rds")

# now for the full stochastic imputations
files <- dir(path = "../output/", pattern = "05_12")

imps <- paste0("../output/", files, sep = "") %>%
  map_dfr(read_rds)

write_rds(imps, "../output/05_multiple_imputation_full_stochastic.rds")
