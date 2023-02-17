library(tidyverse)

set.seed(74838)

n <- 2500

# Create simulation conditions

conditions <- expand_grid(
                          mu_u_n = c(0), 
                          sd_u_n = c(100, 50))

conditions

# Write function for data generation

genesis <- function(mu_u_n, sd_u_n, ...){
  x <- tibble(
    id = seq(1, n, 1),
    x_b = truncnorm::rtruncnorm(n, mean = 1100, sd = 350, a = 300, b = 2500),
    female = rbinom(n, size = 1, prob = 0.5),
    age = rnorm(n, mean = 55, sd = 10) %>% round(., digits = 0),
    age_trunc = if_else(age < 20, 20, if_else(age >= 95, 95, age)),
    age_centered = scale(age_trunc, scale = FALSE) %>% round(., digits = 1) %>% as.vector(),
    x_b_c = scale(x_b, scale = FALSE) %>% as.numeric()
  ) %>%
    group_by(id) %>%
    mutate(
      x_f = truncnorm::rtruncnorm(1, mean = (1120 + 0.1 * x_b_c - 5 * female), sd = 300, a = 300, b = 2500), # PWV after 5 years
      
      # Create two different measurements of PWV, with measurement error, where the distribution is different at the different time points, due to different machines.
    
      
      w_f_n = truncnorm::rtruncnorm(1, mean = x_f + mu_u_n, sd = sd_u_n, a = 300, b = 2500),
      
      x_diff = x_f - x_b,
      w_diff = w_f_n - x_b
    ) %>% 
    ungroup() %>%
    mutate(
      x_diff_c = scale(x_diff, scale = FALSE) %>% as.numeric(),
      w_diff_c = scale(w_diff, scale = FALSE) %>% as.numeric(),
      sampled_for_calibration = rbinom(n, size = 1, prob = 50 / n)
    ) %>%
    group_by(id) %>%
    mutate(
      brain_volume = rnorm(1, mean = 1000 + -0.2 * x_diff_c + -125.217 * female + -4.267 * age_centered, sd = 107)  # Might want to truncate this. Random variation is so small compared to location that it may not be a big deal.
    ) %>%
    ungroup()
  
  return(x)
}

# Create simulated data

sims <- 
  expand_grid(iteration = seq(1, 1000, 1),
              conditions) %>%
  group_by(iteration, mu_u_n, sd_u_n) %>%
  nest() %>%
  mutate(
    df = pmap(list(mu_u_n, sd_u_n), ~genesis(mu_u_n, sd_u_n) %>%
                select(id, x_b, x_f, x_diff, x_diff_c, w_f_n, sampled_for_calibration, age_trunc, age_centered, female, brain_volume))
  )

dir.create("../data/", showWarnings = FALSE)
write_rds(sims, file = "../data/01_simulated_data.rds")
