library(tidyverse)

results <- read_rds("../output/08_results.rds")

re_summary <- results %>%
  group_by(mu_u_n, sd_u_n, term, method) %>%
  summarise(
    empirical_standard_error = sd(estimate)
  ) %>%
  mutate(relative_efficiency = (1 / empirical_standard_error ^ 2) / (1 / last(empirical_standard_error) ^ 2)) %>%
  ungroup() %>%
  mutate(
    method = factor(method) %>% fct_recode("Regression calibration (sandwich)" = "snipe (sandwich)", "Naive" = "naive", "Multiple imputation" = "multiple imputation", "Complete case" = "complete case", "True" = "true") %>%
      fct_relevel("Naive", "Complete case", "Regression calibration (sandwich)", "Multiple imputation", "True")
  )

p <- re_summary %>%
  filter(term == "w_diff_c") %>%
  ggplot(aes(x = relative_efficiency, y = method, color = factor(sd_u_n))) +
  geom_point(position = position_dodge(0.8)) +
  labs(x = "Relative efficiency of estimate for association of interest", 
       y = "", 
       title = str_wrap("Relative efficiency of each method as measurement error at follow-up changes"), 30) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  theme(
    plot.title.position = "plot"
  ) +
  scale_color_manual(values = c("#03244d", "#dd550c", "#496e9c"), name = str_wrap("Measurement error at follow-up (cm/s)", 10))

ggsave("../figs/10_relative_efficiency_dotplot.pdf", p, width = 7, height = 4, units = "in")
