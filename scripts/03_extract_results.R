library(tidyverse)
library(ggsci)
library(rstan)
library(tidybayes)

fit_true <- read_rds("../output/02_simple_lm.rds")
fit_obs <- read_rds("../output/02_simple_lm_observed.rds")
fit_calib <- read_rds("../output/02_simple_lm_calib.rds")
fit_bayes <- read_rds("../output/02_bayes_mem.rds")

bias <- bind_rows(fit_true %>% select(iteration, fits) %>% mutate(method = "true"),
                  fit_obs %>% select(iteration, fits) %>% mutate(method = "observed"),
                  fit_calib %>% select(iteration, fits) %>% mutate(method = "calibrated")) %>%
  mutate(
    bhat_int = map_dbl(fits, ~coefficients(.)[1]),
    bhat_pwv = map_dbl(fits, ~coefficients(.)[2]),
    bhat_female = map_dbl(fits, ~coefficients(.)[3]),
    hat_sigma = map_dbl(fits, ~sigma(.))
  ) %>%
  select(-fits) %>%
  pivot_longer(cols = contains("hat"),
               names_to = "term",
               values_to = "estimate") %>%
  ungroup()

bias_bayes <- fit_bayes %>%
  mutate(
    coefficients = map(fits, ~extract(.) %>% 
                  as_tibble() %>%
                  summarise_all(mean)),
    bhat_int = map_dbl(coefficients, ~select(., beta_0) %>% as.numeric()),
    bhat_pwv = map_dbl(coefficients, ~select(., beta_1) %>% as.numeric()),
    bhat_female = map_dbl(coefficients, ~select(., beta) %>% as.numeric()),
    hat_sigma = map_dbl(coefficients, ~select(., sigma) %>% as.numeric()),
  ) %>%
  select(-fits, -df, -data_stan, -coefficients) %>%
  pivot_longer(cols = contains("hat"),
               names_to = "term",
               values_to = "estimate") %>%
  mutate(method = "bayes") %>%
  ungroup()

bias_combined <- bind_rows(bias, bias_bayes) %>%
  group_by(method, term) %>%
  summarise(mean_estimate = mean(estimate)) %>%
  mutate(
    true_value = case_when(
      term == "bhat_int" ~ 1100,
      term == "bhat_pwv" ~ 0.1,
      term == "bhat_female" ~ -5,
      term == "hat_sigma" ~ 50
    ),
    bias = mean_estimate - true_value,
    percent_bias = (mean_estimate - true_value) / true_value * 100
  )

p_bias <- bias_combined %>%
  mutate(term = factor(term) %>% 
           fct_recode("Residual standard deviation" = "hat_sigma", "Intercept" = "bhat_int", "Female" = "bhat_female", "PWV at visit 1" = "bhat_pwv") %>% 
           fct_reorder(percent_bias),
         method = factor(method) %>% fct_recode("True value" = "true",
                                                "Observed value" = "observed",
                                                "Calibrated value" = "calibrated",
                                                "Bayes measurement error" = "bayes")) %>%
  ggplot(aes(x = percent_bias, y = term, color = method)) +
  geom_point(position = position_dodge(0.3)) +
  scale_color_nejm(name = "") +
  theme_classic() +
  labs(
    x = "Percent bias (%)",
    y = "",
    title = "Percent bias for each regression parameter and\nresidual standard deviation, by data analyzed"
  ) +
  theme(
    plot.title.position = "plot"
  )

ggsave(filename = "../figs/03_bias_plot.png", p_bias)

coverage <- bind_rows(fit_true %>% select(iteration, fits) %>% mutate(method = "true"),
                      fit_obs %>% select(iteration, fits) %>% mutate(method = "observed"),
                      fit_calib %>% select(iteration, fits) %>% mutate(method = "calibrated")) %>%
  mutate(
    conf_intervals = map(fits, ~confint(.)),
    int_low = map_dbl(conf_intervals, ~.[1, 1]),
    int_high = map_dbl(conf_intervals, ~.[1, 2]),
    pwv_low = map_dbl(conf_intervals, ~.[2, 1]),
    pwv_high = map_dbl(conf_intervals, ~.[2, 2]),
    female_low = map_dbl(conf_intervals, ~.[3, 1]),
    female_high = map_dbl(conf_intervals, ~.[3, 2]),
  ) %>%
  select(-fits, -conf_intervals) %>%
  pivot_longer(cols = contains("_"),
                                             names_to = "term",
                                             values_to = "estimate") %>%
  ungroup()

coverage_bayes <- fit_bayes %>%
  mutate(
    conf_int = map(fits, ~spread_draws(., beta_0, beta, beta_1) %>% 
                         mean_qi()),
    int_low = map_dbl(conf_int, ~select(., beta_0.lower) %>% as.numeric()),
    int_low = map_dbl(conf_int, ~select(., beta_0.upper) %>% as.numeric()),
    pwv_low = map_dbl(conf_int, ~select(., beta_1.lower) %>% as.numeric()),
    pwv_high = map_dbl(conf_int, ~select(., beta_1.upper) %>% as.numeric()),
    female_low = map_dbl(conf_int, ~select(., beta.lower) %>% as.numeric()),
    female_high = map_dbl(conf_int, ~select(., beta.upper) %>% as.numeric())
    ) %>%
  select(-fits, -conf_int, -df, -data_stan) %>%
  pivot_longer(cols = contains("_"),
               names_to = "term",
               values_to = "estimate") %>%
  ungroup() %>%
  mutate(
    method = "bayes"
  )

coverage_combined <- bind_rows(coverage, coverage_bayes) %>%
  separate(term, into = c("term", "ci_tail"), sep = "_") %>%
  pivot_wider(names_from = "ci_tail", values_from = "estimate") %>%
  mutate(
    true_value = case_when(
      term == "int" ~ 1100,
      term == "pwv" ~ 0.1,
      term == "female" ~ -5
    ),
    covered = if_else(true_value > low & true_value < high, 1, 0)
  ) %>%
  group_by(method, term) %>%
  summarise(coverage = mean(covered))


p_coverage <- coverage_combined %>%
  mutate(term = factor(term) %>% 
           fct_recode("Intercept" = "int", "Female" = "female", "PWV at visit 1" = "pwv") %>% 
           fct_reorder(coverage),
         method = factor(method) %>% fct_recode("True value" = "true",
                                                "Observed value" = "observed",
                                                "Calibrated method" = "calibrated",
                                                "Bayes measurement error")) %>%
  ggplot(aes(x = coverage, y = method)) +
  geom_point(position = position_dodge(0.3)) +
  geom_vline(xintercept = 0.95, linetype = "dashed") +
  facet_wrap(~ term, scales = "free") +
  theme_classic() +
  labs(
    x = "Percent coverage (%)",
    y = "",
    title = "Percent coverage for each regression parameter, by data analyzed"
  ) +
  theme(
    plot.title.position = "plot"
  )

ggsave(filename = "../figs/03_coverage_plot.png", p_coverage)
