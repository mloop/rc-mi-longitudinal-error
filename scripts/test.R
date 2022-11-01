library(tidyverse)
library(broom)
library(rsample)
library(furrr)
library(tictoc)

sims <- read_rds("../data/01_simulated_data.rds") %>%
  ungroup() %>%
  slice(1:300)

# Fit calibration bootstrap function
fit_calib_on_bootstrap <- function(split) {
  lm(w_f_o ~ w_f_n, analysis(split))
}

# Perform "calibration method" at visit 2, then use predicted values on new machine using data on old machine
fit <- sims %>%
  mutate(
    df  = map(df, ~ungroup(.x)),
    calib_fit = map(df, ~lm(w_f_o ~ w_f_n, data = filter(., sampled_for_calibration == 1))),  # Perform calibration study
    
    #####
    df_calib = map2(df, calib_fit, ~modelr::add_predictions(.x, model = .y) %>%
                      
                      ########
                    mutate(
                      w_diff = if_else(sampled_for_calibration == 0, pred, w_f_o) - w_b_o,  # use the predicted follow up values on the old machine if not in the calibration study; otherwise use the observed values
                      w_diff_c = scale(w_diff, scale = FALSE)  %>% as.numeric()
                    )
    ),
    fits = map(df_calib, ~lm(brain_volume ~ w_diff_c + female + age_centered, data = .) %>%
                 tidy()
    )
    
  ) %>%
  select(-calib_fit, -df, -df_calib) %>%
  unnest(fits)

fit_boot_dfs <- sims %>%
  mutate(
    df  = map(df, ~ungroup(.x)),
    calib_df = map(df, ~filter(., sampled_for_calibration == 1))
  )

tic()
fit_boot_dfs_boot <- fit_boot_dfs %>%
  mutate(
    calib_boot_lms = map2(calib_df, df, ~bootstraps(.x, times = 200, apparent = TRUE) %>%
                            mutate(
                              calib_boot_lm = map(splits, fit_calib_on_bootstrap)
                            ))
  )
toc()

tic()
set.seed(928374)
plan(multisession, workers = 14)

fit_boot_dfs_boot <- fit_boot_dfs %>%
  mutate(
    calib_boot_lms = future_map2(calib_df, df, ~bootstraps(.x, times = 200, apparent = TRUE) %>%
                            mutate(
                              calib_boot_lm = map(splits, fit_calib_on_bootstrap)
                            ),
                            .options = furrr_options(seed = TRUE))
  )

toc()