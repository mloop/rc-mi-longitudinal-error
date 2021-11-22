library(tidyverse)

sims <- read_rds("../data/01_simulated_data.rds")

sims %>%
  mutate(
  mean_diff = map_dbl(df, ~mean(.$measured_diff)),
  sd_diff = map_dbl(df, ~sd(.$measured_diff))
  ) %>%
  summarise(
    m_diff = mean(mean_diff),
    sd_diff = mean(sd_diff)
  )
