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
fit_true %>% write_rds(., file = "../output/2021-01-04/02_simple_lm.rds")

# Fit simple linear regression model to observed values
fit_obs <- sims %>%
  mutate(
    fits = map(df, ~lm(pwv_visit2_measured ~ pwv_visit1_measured + female, data = .))
  )
fit_obs %>% write_rds(file = "../output/2021-01-04/02_simple_lm_observed.rds")

# Fit stan model
fit_stan <- sims %>%
  mutate(
    df_dat = map(df, ~compose_data(select(., pwv_visit1_measured, pwv_visit2_measured, female))),
    fits = map(df_dat, ~stan(file = "02_model.stan", data = .,
                             iter = 5000,
                             chains = 8,
                             include = TRUE,
                             pars = c("mu_u", "sigma", "beta", "beta_1", "mu_pwv_1", "sigma_pwv_1")) %>%
                 as.data.frame() %>%
                 tibble() %>%
                 select(-starts_with("lp")))
  )

write_rds(fit_stan, file = "../output/2021-01-25/02_stan_model.rds")
