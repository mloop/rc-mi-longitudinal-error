library(tidyverse)
library(ggsci)
library(rstan)
library(tidybayes)

fit_true <- read_rds("../output/02_simple_lm.rds")
fit_obs <- read_rds("../output/02_simple_lm_observed.rds")
fit_calib <- read_rds("../output/02_simple_lm_calib.rds")
fit_bayes <- read_rds("../output/02_bayes_mem.rds")

bias <- bind_rows(fit_true %>% select(iteration, me_reduction, fits) %>% mutate(method = "true"),
                  fit_obs %>% select(iteration, me_reduction, fits) %>% mutate(method = "observed"),
                  fit_calib %>% select(iteration, me_reduction, fits) %>% mutate(method = "calibrated")) %>%
  unnest(fits) %>%
  select(-p.value, -statistic) %>%
  ungroup()

bias_bayes <- fit_bayes %>%
  mutate(
    coefficients = map(fits, ~extract(.) %>% 
                  as_tibble() %>%
                  summarise_all(mean)),
    bhat_int = map_dbl(coefficients, ~select(., beta_0) %>% as.numeric()),
    bhat_diff_c = map_dbl(coefficients, ~select(., beta_diff_c) %>% as.numeric()),
    bhat_female = map_dbl(coefficients, ~select(., beta_female) %>% as.numeric()),
    bhat_age_c = map_dbl(coefficients, ~select(., beta_age_c) %>% as.numeric()),
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
  filter(term != "hat_sigma") %>%
  summarise(mean_estimate = mean(estimate)) %>%
  mutate(
    term = if_else(str_detect(term, "diff"), "diff_c", term),
    term = if_else(term == "bhat_age_c", "age_centered", term),
    term = if_else(term == "bhat_int", "(Intercept)", term),
    term = if_else(term == "bhat_female", "female", term),
    true_value = case_when(
      term == "(Intercept)" ~ 1000,
      term == "diff_c" ~ -0.2,
      term == "female" ~ -125.217,
      term == "age_centered" ~ -4.267,
      term == "hat_sigma" ~ 107
    ),
    bias = mean_estimate - true_value,
    percent_bias = (mean_estimate - true_value) / true_value * 100
  )

p_bias <- bias_combined %>%
  mutate(term = factor(term) %>% 
           fct_recode("Intercept" = "(Intercept)", "Female" = "female", "PWV difference (centered)" = "diff_c", "Age (centered)" = "age_centered") %>% 
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

ggsave(filename = "../figs/03_bias_plot.pdf", p_bias)

coverage <- bind_rows(fit_true %>% select(iteration, fits) %>% mutate(method = "true"),
                      fit_obs %>% select(iteration, fits) %>% mutate(method = "observed"),
                      fit_calib %>% select(iteration, fits) %>% mutate(method = "calibrated")) %>%
  mutate(
    conf_intervals = map(fits, ~confint(.)),
    int_low = map_dbl(conf_intervals, ~.[1, 1]),
    int_high = map_dbl(conf_intervals, ~.[1, 2]),
    diff_c_low = map_dbl(conf_intervals, ~.[2, 1]),
    diff_c_high = map_dbl(conf_intervals, ~.[2, 2]),
    female_low = map_dbl(conf_intervals, ~.[3, 1]),
    female_high = map_dbl(conf_intervals, ~.[3, 2]),
    age_c_low = map_dbl(conf_intervals, ~.[4, 1]),
    age_c_high = map_dbl(conf_intervals, ~.[4, 2])
  ) %>%
  select(-fits, -conf_intervals) %>%
  pivot_longer(cols = contains("_"),
                                             names_to = "term",
                                             values_to = "estimate") %>%
  ungroup()

coverage_bayes <- fit_bayes %>%
  mutate(
    conf_int = map(fits, ~spread_draws(., beta_0, beta_diff_c, beta_female, beta_age_c) %>% 
                         mean_qi()),
    int_low = map_dbl(conf_int, ~select(., beta_0.lower) %>% as.numeric()),
    int_high = map_dbl(conf_int, ~select(., beta_0.upper) %>% as.numeric()),
    diff_c_low = map_dbl(conf_int, ~select(., beta_diff_c.lower) %>% as.numeric()),
    diff_c_high = map_dbl(conf_int, ~select(., beta_diff_c.upper) %>% as.numeric()),
    female_low = map_dbl(conf_int, ~select(., beta_female.lower) %>% as.numeric()),
    female_high = map_dbl(conf_int, ~select(., beta_female.upper) %>% as.numeric()),
    age_c_low = map_dbl(conf_int, ~select(., beta_age_c.lower) %>% as.numeric()),
    age_c_high = map_dbl(conf_int, ~select(., beta_age_c.upper) %>% as.numeric()),
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
      term == "bhat_int" ~ 1000,
      term == "bhat_diff_c" ~ -0.2,
      term == "bhat_female" ~ -125.217,
      term == "bhat_age_c" ~ -4.267
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
                                                "Bayes measurement error" = "bayes")) %>%
  ggplot(aes(x = coverage, y = method)) +
  geom_point(position = position_dodge(0.3)) +
  geom_vline(xintercept = 0.95, linetype = "dashed") +
  facet_wrap(~ term) +
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

ci_width_combined <- bind_rows(coverage, coverage_bayes) %>%
  separate(term, into = c("term", "ci_tail"), sep = "_") %>%
  pivot_wider(names_from = "ci_tail", values_from = "estimate") %>%
  mutate(
    diff = abs(high - low)
  ) %>%
  group_by(method, term) %>%
  summarise(
    mean_width = mean(diff)
  )

p_width <- ci_width_combined %>%
  mutate(term = factor(term) %>% 
           fct_recode("Intercept" = "int", "Female" = "female", "PWV at visit 1" = "pwv") %>% 
           fct_reorder(mean_width),
         method = factor(method) %>% fct_recode("True value" = "true",
                                                "Observed value" = "observed",
                                                "Calibrated method" = "calibrated",
                                                "Bayes measurement error" = "bayes")) %>%
  ggplot(aes(x = mean_width, y = method)) +
  geom_point(position = position_dodge(0.3)) +
  facet_wrap(~ term, scales = "free_x") +
  theme_classic() +
  labs(
    x = "Mean width of confidence interval",
    y = "",
    title = "Mean width of confidence interval"
  ) +
  theme(
    plot.title.position = "plot"
  )

ggsave(filename = "../figs/03_ci_width_plot.png", p_width)
