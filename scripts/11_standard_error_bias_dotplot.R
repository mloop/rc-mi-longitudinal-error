library(tidyverse)

results <- read_rds("../output/08_results.rds")

se_summary <- results %>%
  group_by(mu_u_n, sd_u_n, term, method) %>%
  summarise(
    empirical_standard_error = sd(estimate),
    avg_se = mean(std.error),
    se_diff = avg_se - empirical_standard_error,
    se_diff_percent = se_diff / empirical_standard_error * 100
  ) %>%
  ungroup() %>%
  mutate(
    method = factor(method) %>% fct_recode("Regression calibration (sandwich)" = "snipe (sandwich)", "Naive" = "naive", "Multiple imputation" = "multiple imputation", "Complete case" = "complete case", "True" = "true") %>%
      fct_relevel("Naive", "Complete case", "Regression calibration (sandwich)", "Multiple imputation", "True")
  )

p <- se_summary %>%
  filter(term == "w_diff_c") %>%
  ggplot(aes(x = se_diff_percent, y = method, color = factor(sd_u_n))) +
  geom_point(position = position_dodge(0.8)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = str_wrap("Relative difference: (mean standard error - empirical standard error)/empirical standard error", 40), 
       y = "", 
       title = str_wrap("Relative difference between mean standard error and empirical standard error for each method as measurement error at follow-up changes", width = 80)) +
  scale_color_manual(values = c("#03244d", "#dd550c", "#496e9c"), name = str_wrap("Measurement error at follow-up (cm/s)", 10), breaks = c(150, 100, 50)) +
  theme(
    plot.title.position = "plot"
  )

ggsave("../figs/11_se_bias_dotplot.pdf", p, width = 7, height = 4, units = "in")
