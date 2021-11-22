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
  group_by(me_reduction, method, term) %>%
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

write_tsv(bias_combined, file = "../output/03_bias_combined.txt")

p_bias <- bias_combined %>%
  mutate(term = factor(term) %>% 
           fct_recode("Intercept" = "(Intercept)", "Female" = "female", "PWV difference (centered)" = "diff_c", "Age (centered)" = "age_centered") %>% 
           fct_reorder(percent_bias),
         method = factor(method) %>% fct_recode("True value" = "true",
                                                "Observed value" = "observed",
                                                "Calibrated value" = "calibrated",
                                                "Bayes measurement error" = "bayes"),
         me_reduction = factor(me_reduction) %>% fct_recode("75% more precise" = "0.25", "50% more precise" = "0.5", "10% more precise" = "0.9")) %>%
  ggplot(aes(x = percent_bias, y = term, color = method)) +
  geom_point(position = position_dodge(0.3)) +
  scale_color_manual(name = "", values = c("red", "blue", "orange", "black")) +
  theme_classic() +
  facet_wrap(~ me_reduction, ncol = 1) +
  labs(
    x = "Percent bias (%)",
    y = "",
    title = "Percent bias for each regression parameter by method of data analysis and\nhow much more precise the newer machine is"
  ) +
  theme(
    plot.title.position = "plot"
  )

ggsave(filename = "../figs/03_bias_plot.pdf", p_bias, width = 7, height = 7, units = "in")

coverage <- bind_rows(fit_true %>% select(iteration, me_reduction, fits) %>% mutate(method = "true"),
                      fit_obs %>% select(iteration, me_reduction, fits) %>% mutate(method = "observed"),
                      fit_calib %>% select(iteration, me_reduction, fits) %>% mutate(method = "calibrated")) %>%
  unnest(fits) %>%
  select(-p.value, -statistic) %>%
  ungroup() %>%
  mutate(
    low = estimate - 1.96 * std.error,
    high = estimate + 1.96 * std.error
  )

coverage_bayes <- fit_bayes %>%
  mutate(
    conf_int = map(fits, ~gather_draws(., beta_0, beta_diff_c, beta_female, beta_age_c) %>%
                     ungroup() %>%
                     group_by(.variable) %>%
                     summarise(
                       low = quantile(.value, 0.025),
                       high = quantile(.value, 0.975)
                     ))
    ) %>%
  select(-fits, -df, -data_stan) %>%
  unnest(conf_int) %>%
  ungroup() %>%
  mutate(
    method = "bayes"
  )

coverage_combined <- bind_rows(coverage, rename(coverage_bayes, term = .variable)) %>%
  mutate(
    term = if_else(str_detect(term, "diff"), "diff_c", term),
    term = if_else(term == "beta_age_c", "age_centered", term),
    term = if_else(term == "beta_int", "(Intercept)", term),
    term = if_else(term == "beta_0", "(Intercept)", term),
    term = if_else(term == "beta_female", "female", term)
  ) %>%
  mutate(
  true_value = case_when(
    term == "(Intercept)" ~ 1000,
    term == "diff_c" ~ -0.2,
    term == "female" ~ -125.217,
    term == "age_centered" ~ -4.267,
    term == "hat_sigma" ~ 107
  ),
    covered = if_else(true_value > low & true_value < high, 1, 0)
  ) %>%
  group_by(me_reduction, method, term) %>%
  summarise(coverage = mean(covered))

write_tsv(coverage_combined, file = "../output/03_coverage_combined.txt")

p_coverage <- coverage_combined %>%
  mutate(term = factor(term) %>% 
           fct_recode("Intercept" = "(Intercept)", "Female" = "female", "PWV difference (centered)" = "diff_c", "Age (centered)" = "age_centered") %>% 
           fct_reorder(coverage),
         method = factor(method) %>% fct_recode("True value" = "true",
                                                "Observed value" = "observed",
                                                "Calibrated value" = "calibrated",
                                                "Bayes measurement error" = "bayes"),
         me_reduction = factor(me_reduction) %>% fct_recode("75% more precise" = "0.25", "50% more precise" = "0.5", "10% more precise" = "0.9")) %>%
  filter(term == "PWV difference (centered)") %>%
  ggplot(aes(x = coverage, y = method)) +
  geom_point(position = position_dodge(0.3)) +
  geom_vline(xintercept = 0.95, linetype = "dashed") +
  facet_wrap(~ me_reduction) +
  theme_classic() +
  labs(
    x = "Percent coverage (%)",
    y = "",
    title = "Percent coverage for each method, by reduction in measurement error"
  ) +
  theme(
    plot.title.position = "plot"
  )

ggsave(filename = "../figs/03_coverage_plot.pdf", p_coverage, width = 9, height = 3, units = "in")

ci_width <- bind_rows(fit_true %>% select(iteration, me_reduction, fits) %>% mutate(method = "true"),
                                  fit_obs %>% select(iteration, me_reduction, fits) %>% mutate(method = "observed"),
                                  fit_calib %>% select(iteration, me_reduction, fits) %>% mutate(method = "calibrated")) %>%
  unnest(fits) %>%
  select(-p.value, -statistic) %>%
  ungroup() %>%
  mutate(
    ci_width = 2 * 1.96 * std.error
  )

ci_width_bayes <- fit_bayes %>%
  mutate(
    ci = map(fits, ~gather_draws(., beta_0, beta_diff_c, beta_female, beta_age_c) %>%
                     ungroup() %>%
                     group_by(.variable) %>%
                     summarise(
                       ci_width = quantile(.value, probs = 0.975) - quantile(.value, probs = 0.025)
                     ))
  ) %>%
  select(-fits, -df, -data_stan) %>%
  unnest(ci) %>%
  ungroup() %>%
  mutate(
    method = "bayes"
  )

ci_width_combined <- bind_rows(ci_width, rename(ci_width_bayes, term = .variable)) %>%
  mutate(
    term = if_else(str_detect(term, "diff"), "diff_c", term),
    term = if_else(term == "beta_age_c", "age_centered", term),
    term = if_else(term == "beta_int", "(Intercept)", term),
    term = if_else(term == "beta_0", "(Intercept)", term),
    term = if_else(term == "beta_female", "female", term)
  ) %>%
  ungroup() %>%
  group_by(me_reduction, method, term) %>%
  summarise(mean_width = mean(ci_width))

write_tsv(ci_width_combined, file = "../output/03_ci_width_combined.txt")

p_width <- ci_width_combined %>%
  mutate(term = factor(term) %>% 
           fct_recode("Intercept" = "(Intercept)", "Female" = "female", "PWV difference (centered)" = "diff_c", "Age (centered)" = "age_centered") %>% 
           fct_reorder(mean_width),
         method = factor(method) %>% fct_recode("True value" = "true",
                                                "Observed value" = "observed",
                                                "Calibrated value" = "calibrated",
                                                "Bayes measurement error" = "bayes"),
         me_reduction = factor(me_reduction) %>% fct_recode("75% more precise" = "0.25", "50% more precise" = "0.5", "10% more precise" = "0.9")) %>%
  filter(term == "PWV difference (centered)") %>%
  ggplot(aes(x = mean_width, y = method)) +
  geom_point(position = position_dodge(0.3)) +
  facet_wrap(~ me_reduction) +
  theme_classic() +
  labs(
    x = "Mean width of confidence / credible interval",
    y = "",
    title = "Mean width of 95% confidence / credible interval"
  ) +
  theme(
    plot.title.position = "plot"
  )

ggsave(filename = "../figs/03_ci_width_plot.pdf", p_width, width = 11, height = 6, units = "in")
