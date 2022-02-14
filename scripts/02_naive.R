library(tidyverse)
library(broom)

sims <- read_rds("../data/01_simulated_data.rds")

dir.create("../output/", showWarnings = FALSE)

# Fit simple linear regression model to observed
fit <- sims %>%
  mutate(
    fits = map(df, ~mutate(., w_diff = w_f_n - w_b_o,
                           w_diff_c = scale(w_diff, scale = FALSE) %>% as.numeric()) %>%
                 lm(brain_volume ~ w_diff_c + female + age_centered, data = .) %>%
                 tidy())
  ) %>%
  select(-df, -data) %>%
  unnest(fits)

fit %>% write_rds(., file = "../output/02_naive.rds")