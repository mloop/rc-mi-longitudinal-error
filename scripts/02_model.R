library(tidyverse)
library(rstan)
library(tidybayes)
 
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

sims <- read_rds("../data/01_simulated_data.rds")

# Fit simple linear regression model to true values
fit_true <- sims %>%
  mutate(
    fits = map(df, ~lm(pwv_visit2 ~ pwv_visit1 + female, data = .))
  )
fit_true %>% write_rds(., file = "../output/02_simple_lm.rds")

# Fit simple linear regression model to observed values
fit_obs <- sims %>%
  mutate(
    fits = map(df, ~lm(pwv_visit2_measured ~ pwv_visit1_measured + female, data = .))
  )
fit_obs %>% write_rds(file = "../output/02_simple_lm_observed.rds")

# Perform "calibration method" at visit 1, then use predicted values to fit model
fit_calib <- sims %>%
  mutate(
    calib_fit = map(df, ~lm(pwv_visit1_measured_calibration ~ pwv_visit1_measured, data = .)),
    df_calib = map2(df, calib_fit, ~modelr::add_predictions(.x, model = .y)),
    fits = map(df_calib, ~lm(pwv_visit2_measured ~ pred + female, data = .))
  ) %>%
  select(-calib_fit)
fit_calib %>% write_rds(file = "../output/02_simple_lm_calib.rds")
