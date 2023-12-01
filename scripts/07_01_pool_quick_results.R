library(tidyverse)

# naive results
files <- dir(path = "../output/naive/", pattern = "02")

boots <- paste0("../output/naive/", files, sep = "") %>%
  map(read_rds)

write_rds(boots, "../output/02_naive.rds")

# complete case results
files <- dir(path = "../output/complete_case/", pattern = "03")

boots <- paste0("../output/complete_case/", files, sep = "") %>%
  map(read_rds)

write_rds(boots, "../output/03_complete_case.rds")

# true/control model results
files <- dir(path = "../output/true/", pattern = "07")

boots <- paste0("../output/true/", files, sep = "") %>%
  map(read_rds)

write_rds(boots, "../output/07_true.rds")