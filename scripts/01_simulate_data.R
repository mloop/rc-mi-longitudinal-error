library(tidyverse)

set.seed(74838)

n <- 2500

# Create true values of PWV
df <- tibble(
  pwv_visit1 = rnorm(n, mean = 1100, sd = 350),
  female = rbinom(n, size = 1, prob = 0.5),

  pwv_visit2 = rnorm(n, mean = (20 + 1 * pwv_visit1 - 5 * female), sd = 10), # PWV after 5 years

# Create two different measurements of PWV, with measurement error, where the distribution is different at the different time points, due to different machines.

  pwv_visit1_measured = pwv_visit1 + rnorm(n, mean = 0, sd = 20),

  pwv_visit2_measured = pwv_visit2 + rnorm(n, mean = 10, sd = 10)
)

write_rds(df, file = "../data/01_simulated_data.rds")
