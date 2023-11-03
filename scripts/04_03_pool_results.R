library(tidyverse)

files <- dir(path = "../output/", pattern = "04_02")

boots <- paste0("../output/", files, sep = "") %>%
  map(read_rds)

write_rds(imps, "../output/04_02_snipe_bootstrap.rds")
