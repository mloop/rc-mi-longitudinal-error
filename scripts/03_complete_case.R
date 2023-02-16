library(tidyverse)
library(broom)

sims <- read_rds("../data/01_simulated_data.rds")

fit <- sims %>%
  mutate(
    fits = map(df, ~filter(., sampled_for_calibration == 1) %>%
                 mutate(w_diff = x_f - x_b,
                           w_diff_c = scale(w_diff, scale = FALSE) %>% as.numeric()) %>%
                 lm(brain_volume ~ w_diff_c + female + age_centered,
                    data = .) %>%
                 tidy())
  ) %>%
  select(-df, -data) %>%
  unnest(fits)

fit %>% write_rds(., file = "../output/03_complete_case.rds")