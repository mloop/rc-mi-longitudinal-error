library(tidyverse)

files <- dir(path = "../output/snipe_boot/", pattern = "04_02")

boots <- paste0("../output/snipe_boot/", files, sep = "") %>%
  map(read_rds)

write_rds(boots, "../output/04_02_snipe_boot.rds")
