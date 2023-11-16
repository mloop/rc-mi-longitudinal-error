i <- Sys.getenv('SLURM_ARRAY_TASK_ID') |> as.numeric()

library(tidyverse)
library(broom)

x <- read_rds(paste0("../data/01_simulated_data_", i, ".rds"))

dir.create("../output/", showWarnings = FALSE)

# Fit simple linear regression model to observed
fit <- x %>%
  mutate(
    fits = map(df, ~mutate(., w_diff = w_f_n - x_b,
                           w_diff_c = scale(w_diff, scale = FALSE) %>% as.numeric()) %>%
                 lm(brain_volume ~ w_diff_c + female + age_centered, data = .) %>%
                 tidy())
  ) %>%
  select(-df, -data) %>%
  unnest(fits)

dir.create("../output/naive/", showWarnings = FALSE)
fit %>% write_rds(., file = paste0("../output/naive/02_naive_", i, ".rds"))