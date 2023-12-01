i <- Sys.getenv('SLURM_ARRAY_TASK_ID') |> as.numeric()

library(tidyverse)
library(broom)

x <- read_rds(paste0("../data/01_simulated_data_", i, ".rds"))

fit <- x %>%
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

dir.create("../output/complete_case/", showWarnings = FALSE)
fit %>% write_rds(., file = paste0("../output/complete_case/03_complete_case_", i, ".rds"))