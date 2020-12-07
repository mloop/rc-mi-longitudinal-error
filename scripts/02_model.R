library(tidyverse)
library(rstan)
library(tidybayes)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

df <- read_rds("../output/01_simulated_data.rds") %>%
  slice(1:100)

df_dat <- compose_data(df)

m <- stan(file = "../output/02_model.stan", data = df_dat)
m
