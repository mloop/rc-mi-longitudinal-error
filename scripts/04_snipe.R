library(tidyverse)
library(broom)
library(rsample)
library(furrr)

sims <- read_rds("../data/01_simulated_data.rds")

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

set.seed(928374)
plan(multicore, workers = 172)

fit_boot_dfs <- sims %>%
  mutate(
    df  = map(df, ~ungroup(.x)),
    calib_df = map(df, ~filter(., sampled_for_calibration == 1))
  )

fit_boot_dfs_boot <- fit_boot_dfs %>%
  mutate(
    calib_boot_lms = future_map2(calib_df, df, ~bootstraps(.x, times = 200, apparent = TRUE) %>%
                       mutate(
                         calib_boot_lm = map(splits, fit_calib_on_bootstrap)
                       ),
                       .options = furrr_options(seed = TRUE))
  ) 

plan(multicore, workers = 172)
fit_boot_preds <- fit_boot_dfs_boot %>%
  unnest(calib_boot_lms) %>%
  mutate(
    calib_boot_preds = future_map2(df, calib_boot_lm, ~modelr::add_predictions(.x, model = .y) %>%
                              mutate(
                                w_diff = if_else(sampled_for_calibration == 0, pred, w_f_o) - w_b_o,  # use the predicted follow up values on the old machine if not in the calibration study; otherwise use the observed values
                                w_diff_c = scale(w_diff, scale = FALSE)  %>% as.numeric()
                              )
                            )
  )

plan(multicore, workers = 172)

fit_boot_se <- fit_boot_preds %>%
  mutate(
    estimate_fits_boot = future_map_dbl(calib_boot_preds, ~lm(brain_volume ~ w_diff_c + female + age_centered, data = .) %>%
                      broom::tidy() %>%
                      filter(term == "w_diff_c") %>%
                      select(estimate) %>%
                      pull()
    )
  ) %>%
  group_by(iteration, mu_u_o, mu_u_n, sd_u_o, sd_u_n) %>%
  summarize(
    boot_se = sd(estimate_fits_boot)
  )

fit_final <- left_join(fit, fit_boot_se, by = c("iteration", "mu_u_o", "mu_u_n", "sd_u_o", "sd_u_n")) %>%
  select(-std.error) %>%  # We are getting rid of the old se and getting the bootstrapped se using the quantile method. WARNING!!!:: The same se will be listed for all of the terms in the model, but that is WRONG. It is only for the w_diff_c term. It is just repeated because I calculated the bootstrapped se for only this term, not for the others.
  
  rename(std.error = boot_se)  # We are renaming the se so that it will play nicely with the downstream code. But this is the bootstrapped se.

fit_final %>% write_rds(file = "../output/04_snipe.rds")
