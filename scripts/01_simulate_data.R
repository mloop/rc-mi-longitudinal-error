library(tidyverse)

set.seed(74838)

n <- 2500

# Create true values of PWV

sims <- 
  tibble(iteration = seq(1, 10, 1)) %>%
  group_by(iteration) %>%
  nest() %>%
  mutate(
    df = map(iteration, ~tibble(
      id = seq(1, n, 1),
  pwv_visit1 = truncnorm::rtruncnorm(n, mean = 1100, sd = 350, a = -1000, b = 20000),
  female = rbinom(n, size = 1, prob = 0.5),
  pwv_visit1_c = scale(pwv_visit1, scale = FALSE) %>% as.numeric()
    ) %>%
    group_by(id) %>%
    mutate(
      pwv_visit2 = truncnorm::rtruncnorm(1, mean = (1120 + 0.1 * pwv_visit1_c - 5 * female), sd = 50, a = -1000, b = 20000), # PWV after 5 years

# Create two different measurements of PWV, with measurement error, where the distribution is different at the different time points, due to different machines.

      pwv_visit1_measured = truncnorm::rtruncnorm(1, mean = pwv_visit1, sd = 112.8, a = -1000, b = 20000) %>% as.numeric(),

  # To use the Hickson vicorder, paper, let's assume the following:
  #   1. the CV within visit was 2.8% (reported in paper)
  #   2. the overall mean aPWV in Hickson is 800 cm/s
  #   3. based on formula for CV, we calculate in-person SD to be 0.028 * 800 = 22.4
      pwv_visit2_measured = truncnorm::rtruncnorm(1, mean = pwv_visit2, sd = 22.4, a = -1000, b = 20000),
  
      pwv_visit1_measured_calibration = truncnorm::rtruncnorm(1, mean = pwv_visit1, sd = 22.4, a = -1000, b = 20000)
                )
%>% ungroup()
            )
) %>%
  select(-data) %>%
  ungroup()

write_rds(sims, file = "../data/01_simulated_data.rds")
