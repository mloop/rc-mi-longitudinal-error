library(tidyverse)
library(rstan)
library(tidybayes)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

df <- read_rds("../data/01_simulated_data.rds")

# Fit simple linear regression model to true values
df
fit <- lm(pwv_visit2 ~ pwv_visit1 + female, data = df)
fit %>% write_rds(., file = "../output/2021-01-04/02_simple_lm.rds")

# Fit simple linear regression model to observed values
fit_obs <- lm(pwv_visit2_measured ~ pwv_visit1_measured + female, data = df)
fit_obs %>% write_rds(file = "../output/2021-01-04/02_simple_lm_observed.rds")

# Fit stan model
df_dat <- compose_data(select(df, pwv_visit1_measured, pwv_visit2_measured, female))

m <- stan(file = "02_model.stan", data = df_dat,
          iter = 5000)  # I know that I've given almost exactly the generative model, so why does it take so long to run? And why is there such variability among the chains?

write_rds(m, file = "../output/2021-01-04/02_stan_model.rds")
