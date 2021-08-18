library(tidyverse)
library(rstan)
library(tidybayes)
 
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

sims <- read_rds("../data/01_simulated_data.rds")

# Fit simple linear regression model to true values
fit_true <- sims %>%
  mutate(
    fits = map(df, ~lm(pwv_visit2 ~ pwv_visit1_c + female, data = .))
  ) %>%
  select(-df)
fit_true %>% write_rds(., file = "../output/02_simple_lm.rds")

# Fit simple linear regression model to observed values
fit_obs <- sims %>%
  mutate(
    fits = map(df, ~lm(pwv_visit2_measured ~ pwv_visit1_measured_c + female, data = .))
  ) %>%
  select(-df)
fit_obs %>% write_rds(file = "../output/02_simple_lm_observed.rds")

# Perform "calibration method" at visit 1, then use predicted values to fit model
fit_calib <- sims %>%
  mutate(
    calib_fit = map(df, ~lm(pwv_visit2_measured ~ pwv_visit2_measured_calibration, data = .)),  # Perform calibration study
    df_calib = map2(df, calib_fit, ~modelr::add_predictions(select(.x, pwv_visit1_measured) %>%  # Get predicted new device measurements on old visit 1 measures
                                                              rename(pwv_visit2_measured_calibration = pwv_visit1_measured), model = .y) %>%
                      rename(pwv_visit1_measured = pwv_visit2_measured_calibration) %>%
                      bind_cols(select(.x, -pwv_visit1_measured), .) %>%
                      mutate(pred_c = scale(pred, scale = FALSE)  %>% as.numeric())),
    fits = map(df_calib, ~lm(pwv_visit2_measured ~ pred_c + female, data = .))
  ) %>%
  select(-calib_fit, -df, -df_calib)
fit_calib %>% write_rds(file = "../output/02_simple_lm_calib.rds")

# Fit Bayesian measurement error model
 
me_mod <- stan_model(file = "02_model.stan")

library(furrr)

plan(multicore, workers = 30)

fit_bayes <- sims %>%
  mutate(
    data_stan = future_map2(df, me_reduction, ~tidybayes::compose_data(.x) %>% list_modify(me_reduction = .y)),
    fits = future_map(data_stan, ~sampling(me_mod, data = ., iter = 2000, chains = 4,
                                    pars = c("mu_pwv_1", 
                                             "sigma_pwv_1", 
                                             "mu_pwv_2",
                                             "sigma_pwv_2",
                                             "beta_0", 
                                             "beta_female", 
                                             "beta_age_c", 
                                             "beta_diff_c",
                                             "sigma"), 
                                    include = TRUE),
          seed = TRUE)
  )

fit_bayes %>% write_rds(file = "../output/02_bayes_mem.rds")
