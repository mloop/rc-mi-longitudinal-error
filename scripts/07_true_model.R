i <- Sys.getenv('SLURM_ARRAY_TASK_ID') |> as.numeric()

library(tidyverse)
library(broom)

x <- read_rds(paste0("../data/01_simulated_data_", i, ".rds"))

# Fit simple linear regression model to observed
fit <- x %>%
  mutate(
    fits = map(df, ~ lm(brain_volume ~ x_diff_c + female + age_centered, data = .) %>%
                 tidy())
  ) %>%
  select(-df, -data) %>%
  unnest(fits)

dir.create("../output/true/", showWarnings = FALSE)
fit %>% write_rds(., file = paste0("../output/true/07_true_model_", i, ".rds"))