library(tidyverse)

results <- read_rds("../output/08_results.rds")

se_summary <- results %>%
  group_by(mu_u_o, mu_u_n, sd_u_o, sd_u_n, term, method) %>%
  summarise(
    empirical_standard_error = sd(estimate),
    avg_se = mean(std.error),
    se_diff = avg_se - empirical_standard_error,
    se_diff_percent = se_diff / empirical_standard_error * 100
  ) %>%
  ungroup() %>%
  mutate(
    device_bias = case_when(
      mu_u_n == mu_u_o ~ "Both devices biased by +10 cm/s",
      mu_u_n > mu_u_o ~ "New device biased by +15 cm/s, old by +10 cm/s",
      mu_u_n < mu_u_o ~ "New device biased by +10 cm/s, old by +15 cm/s"
    ) %>% factor() %>% fct_relevel("New device biased by +10 cm/s, old by +15 cm/s", "Both devices biased by +10 cm/s"),
    measurement_error = case_when(
      sd_u_n == sd_u_o ~ "Measurement error equal (113 cm/s)",
      sd_u_n > sd_u_o ~ "New measurement error 113 cm/s, old 50 cm/s",
      sd_u_n < sd_u_o ~ "New measurement error 50 cm/s, old 113 cm/s"
    ) %>% factor() %>% fct_relevel("New measurement error 50 cm/s, old 113 cm/s", "Measurement error equal (113 cm/s)"),
    method = factor(method) %>% fct_recode("Regression calibration (sandwich)" = "snipe (sandwich)", "Naive" = "naive", "Multiple imputation" = "multiple imputation", "Complete case" = "complete case", "True" = "true") %>%
      fct_relevel("Naive", "Complete case", "Regression calibration (sandwich)", "Multiple imputation", "True")
  )

p <- se_summary %>%
  filter(term == "w_diff_c") %>%
  ggplot(aes(x = se_diff_percent, y = method)) +
  geom_point() +
  labs(x = "Relative difference: (mean standard error - empirical standard error)/empirical standard error", 
       y = "Method", 
       title = str_wrap("Relative difference between mean standard error and empirical standard error for each method as measurement error and bias of each device changes", width = 80)) +
  facet_grid(device_bias ~ measurement_error,
             labeller = labeller(device_bias = label_wrap_gen(width = 25),
                                 measurement_error = label_wrap_gen(width = 25))) +
  ggsci::scale_color_aaas()

ggsave("../figs/11_se_bias_dotplot.pdf", p, width = 12, height = 6, units = "in")
