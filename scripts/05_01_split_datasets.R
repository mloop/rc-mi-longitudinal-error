library(tidyverse)

sims <- read_rds("../data/01_simulated_data.rds") %>%
  select(-data) %>%
  group_by(mu_u_n, sd_u_n) %>%
  arrange(mu_u_n, sd_u_n)


for(i in 1:5000){
  x <- filter(sims, iteration == i)
  write_rds(x, file = paste0("../output/05_01_", i, ".rds", sep = ""))
}