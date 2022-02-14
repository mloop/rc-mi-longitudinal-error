library(tidyverse)
library(broom)

sims <- read_rds("../data/01_simulated_data.rds")

# Fit simple linear regression model to observed
fit <- sims %>%
  mutate(
    fits = map(df, ~ lm(brain_volume ~ x_diff_c + female + age_centered, data = .) %>%
                 tidy())
  ) %>%
  select(-df, -data) %>%
  unnest(fits)

fit %>% write_rds(., file = "../output/07_true_model.rds")