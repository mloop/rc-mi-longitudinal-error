library(tidyverse)

set.seed(74838)

n <- 2500

# Create true values of PWV

sims <- 
  expand_grid(iteration = seq(1, 2000, 1),
         me_reduction = c(0.9, 0.5, 0.25)) %>%
  group_by(iteration, me_reduction) %>%
  nest() %>%
  mutate(
    df = map2(iteration, me_reduction, ~tibble(
      id = seq(1, n, 1),
  pwv_visit1 = truncnorm::rtruncnorm(n, mean = 1100, sd = 350, a = 300, b = 2500),
  female = rbinom(n, size = 1, prob = 0.5),
  pwv_visit1_c = scale(pwv_visit1, scale = FALSE) %>% as.numeric()
    ) %>%
    group_by(id) %>%
    mutate(
      pwv_visit2 = truncnorm::rtruncnorm(1, mean = (1120 + 0.1 * pwv_visit1_c - 5 * female), sd = 50, a = 300, b = 2500), # PWV after 5 years

# Create two different measurements of PWV, with measurement error, where the distribution is different at the different time points, due to different machines.

      pwv_visit1_measured = truncnorm::rtruncnorm(1, mean = pwv_visit1, sd = 112.8, a = 300, b = 2500) %>% as.numeric(),

      pwv_visit2_measured = truncnorm::rtruncnorm(1, mean = pwv_visit2, sd = 112.8 * .y, a = 300, b = 2500),
  
      pwv_visit2_measured_calibration = truncnorm::rtruncnorm(1, mean = pwv_visit2, sd = 112.8, a = 300, b = 2500),
      
                ) %>% 
    ungroup() %>%
    mutate(
      pwv_visit1_measured_c = scale(pwv_visit1_measured, scale = FALSE) %>% as.numeric(),
      pwv_visit2_measured_calibration_c = scale(pwv_visit2_measured_calibration, scale = FALSE) %>% as.numeric()
    )
            )
) %>%
  select(-data) %>%
  ungroup()

write_rds(sims, file = "../data/01_simulated_data.rds")
