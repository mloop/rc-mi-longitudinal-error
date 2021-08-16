library(tidyverse)

set.seed(74838)

n <- 2500

# Create true values of PWV

sims <- 
  expand_grid(iteration = seq(1, 2, 1),
         me_reduction = c(0.9, 0.5, 0.25)) %>%
  group_by(iteration, me_reduction) %>%
  nest() %>%
  mutate(
    df = map2(iteration, me_reduction, ~tibble(
      id = seq(1, n, 1),
  pwv_visit1 = truncnorm::rtruncnorm(n, mean = 1100, sd = 350, a = 300, b = 2500),
  female = rbinom(n, size = 1, prob = 0.5),
  age = rnorm(n, mean = 55, sd = 10) %>% round(., digits = 0),
  age_trunc = if_else(age < 20, 20, if_else(age >= 95, 95, age)),
  age_centered = scale(age_trunc, scale = FALSE) %>% round(., digits = 1) %>% as.vector(),
  pwv_visit1_c = scale(pwv_visit1, scale = FALSE) %>% as.numeric()
    ) %>%
    group_by(id) %>%
    mutate(
      pwv_visit2 = truncnorm::rtruncnorm(1, mean = (1120 + 0.1 * pwv_visit1_c - 5 * female), sd = 50, a = 300, b = 2500), # PWV after 5 years

# Create two different measurements of PWV, with measurement error, where the distribution is different at the different time points, due to different machines.

      pwv_visit1_measured = truncnorm::rtruncnorm(1, mean = pwv_visit1, sd = 112.8, a = 300, b = 2500) %>% as.numeric(),

      pwv_visit2_measured = truncnorm::rtruncnorm(1, mean = pwv_visit2, sd = 112.8 * .y, a = 300, b = 2500),
  
      pwv_visit2_measured_calibration = truncnorm::rtruncnorm(1, mean = pwv_visit2, sd = 112.8, a = 300, b = 2500),
  
      true_diff = pwv_visit2 - pwv_visit1,
      measured_diff = pwv_visit2_measured - pwv_visit1_measured
      
      
      
                ) %>% 
    ungroup() %>%
    mutate(
      pwv_visit1_measured_c = scale(pwv_visit1_measured, scale = FALSE) %>% as.numeric(),
      pwv_visit2_measured_calibration_c = scale(pwv_visit2_measured_calibration, scale = FALSE) %>% as.numeric(),
      true_diff_c = scale(true_diff, scale = FALSE) %>% as.numeric(),
      measured_diff_c = scale(measured_diff, scale = FALSE) %>% as.numeric()
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
