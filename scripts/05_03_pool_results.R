library(tidyverse)

files <- dir(path = "../output/mi_pmm/", pattern = "05_02")

imps <- paste0("../output/mi_pmm/", files, sep = "") %>%
  map_dfr(read_rds)

write_rds(imps, "../output/05_multiple_imputation.rds")

# now for the full stochastic imputations
files <- dir(path = "../output/mi_full_stochastic/", pattern = "05_12")

imps <- paste0("../output/mi_full_stochastic/", files, sep = "") %>%
  map_dfr(read_rds)

write_rds(imps, "../output/05_multiple_imputation_full_stochastic.rds")
