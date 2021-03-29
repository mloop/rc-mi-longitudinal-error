library(tidyverse)
library(ggsci)

fit_true <- read_rds("../output/02_simple_lm.rds")
fit_obs <- read_rds("../output/02_simple_lm_observed.rds")
fit_calib <- read_rds("../output/02_simple_lm_calib.rds")

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
  ungroup() %>%
  group_by(method, term) %>%
  summarise(mean_estimate = mean(estimate)) %>%
  mutate(
    true_value = case_when(
        term == "bhat_int" ~ 20,
        term == "bhat_pwv" ~ 1,
        term == "bhat_female" ~ -5,
        term == "hat_sigma" ~ 10
      ),
    bias = mean_estimate - true_value,
    percent_bias = (mean_estimate - true_value) / true_value * 100
)

p_bias <- bias %>%
  mutate(term = factor(term) %>% 
           fct_recode("Residual standard deviation" = "hat_sigma", "Intercept" = "bhat_int", "Female" = "bhat_female", "PWV at visit 1" = "bhat_pwv") %>% 
           fct_reorder(percent_bias),
         method = factor(method) %>% fct_recode("True value" = "true",
                                                "Observed value" = "observed",
                                                "Calibrated method" = "calibrated")) %>%
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
  ungroup() %>%
  separate(term, into = c("term", "ci_tail"), sep = "_") %>%
  pivot_wider(names_from = "ci_tail", values_from = "estimate") %>%
  mutate(
    true_value = case_when(
      term == "int" ~ 20,
      term == "pwv" ~ 1,
      term == "female" ~ -5
    ),
    covered = if_else(true_value > low & true_value < high, 1, 0)
  ) %>%
  group_by(method, term) %>%
  summarise(coverage = mean(covered))

p_coverage <- coverage %>%
  mutate(term = factor(term) %>% 
           fct_recode("Intercept" = "int", "Female" = "female", "PWV at visit 1" = "pwv") %>% 
           fct_reorder(coverage),
         method = factor(method) %>% fct_recode("True value" = "true",
                                                "Observed value" = "observed",
                                                "Calibrated method" = "calibrated")) %>%
  ggplot(aes(x = coverage, y = term, color = method)) +
  geom_point(position = position_dodge(0.3)) +
  geom_vline(xintercept = 0.95, linetype = "dashed") +
  scale_color_nejm(name = "") +
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
