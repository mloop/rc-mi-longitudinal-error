library(tidyverse)

set.seed(74838)

n <- 2500

# Create true values of PWV

sims <- 
  expand_grid(iteration = seq(1, 1, 1)) %>%  # 10% reduction in me, 50%, and 75%
  group_by(iteration) %>%
  nest() %>%
  mutate(
    df = map(iteration, ~tibble(
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

      w_b_o = truncnorm::rtruncnorm(1, mean = x_b + 100, sd = 112.8, a = 300, b = 2500) %>% as.numeric(),

      w_f_n = truncnorm::rtruncnorm(1, mean = x_f + 75, sd = 112.8, a = 300, b = 2500),
  
      w_f_o = truncnorm::rtruncnorm(1, mean = x_f + 100, sd = 112.8, a = 300, b = 2500),
  
      true_diff = x_f - x_b,
      measured_diff = w_f_n - w_b_o
      
      
      
                ) %>% 
    ungroup() %>%
    mutate(
      w_b_o_c = scale(w_b_o, scale = FALSE) %>% as.numeric(),
      w_f_n_c = scale(w_f_n, scale = FALSE) %>% as.numeric(),
      true_diff_c = scale(true_diff, scale = FALSE) %>% as.numeric(),
      measured_diff_c = scale(measured_diff, scale = FALSE) %>% as.numeric(),
      sampled_for_calibration = rbinom(n, size = 1, prob = 50 / n)
    ) %>%
  group_by(id) %>%
  mutate(
    brain_volume = rnorm(1, mean = 1000 + -0.2 * true_diff_c + -125.217 * female + -4.267 * age_centered, sd = 107)  # Might want to truncate this. Random variation is so small compared to location that it may not be a big deal.
  )
            )
) %>%
  select(-data) %>%
  ungroup()

dir.create("../data/", showWarnings = FALSE)
write_rds(sims, file = "../data/01_simulated_data.rds")
