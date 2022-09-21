library(tidyverse)
library(broom)
library(mice)
library(furrr)

sims <- read_rds("../data/01_simulated_data.rds")

set.seed(987234)
plan(multicore, workers = 48)

fit <- sims %>%
  mutate(
    imp = future_map(df, ~mutate(., w_f_o = if_else(sampled_for_calibration == 1, w_f_o, NA_real_),
                          w_diff = NA) %>%
                select(w_b_o, w_f_n, w_f_o, age_centered, female, brain_volume) %>%
                mice(m = 100, 
                     printFlag = FALSE) %>%
                mice::complete(action = "long", include = TRUE) %>% 
                as_tibble()
              ),
    
    modified_imp = map(imp, ~mutate(., w_diff = w_f_o - w_b_o) %>%
                         group_by(.imp) %>%
                         mutate(
                           w_diff_c = scale(w_diff, scale = FALSE) %>% as.numeric()
                           )
                         ),
    
    new_mids = map(modified_imp, ~as.mids(.)),
    
    fits = map(new_mids, ~with(., lm(brain_volume ~ w_diff_c + female + age_centered)) %>%
                 pool() %>%
                 tidy())
  ) %>%
  select(-data, -df, -imp, -modified_imp, -new_mids) %>%
  unnest(fits)

fit %>% write_rds(., file = "../output/05_multiple_imputation.rds")
