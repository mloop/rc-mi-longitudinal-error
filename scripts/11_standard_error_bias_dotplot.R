library(tidyverse)

results <- read_rds("../output/08_results.rds")

se_summary <- results %>%
  ungroup() %>%
  group_by(n, calibration_p, sd_u_n, term, method) %>%
  summarize(
    empirical_standard_error = sd(estimate),
    avg_se = mean(std.error)
  ) %>%
  mutate(
    se_diff = avg_se - empirical_standard_error,
    se_diff_percent = (avg_se - empirical_standard_error) / empirical_standard_error,
    method = factor(method) %>%
      fct_recode("Regression calibration (bootstrap)" = "snipe (boot)",
                 "Naive" = "naive",
                 "Multiple imputation (PMM)" = "multiple imputation (pmm)",
                 "Multiple imputation (full stochastic)" = "multiple imputation (full stochastic)",
                 "Complete case" = "complete case", 
                 "Control" = "true") %>%
      fct_relevel("Naive", "Complete case", "Regression calibration (bootstrap)", "Multiple imputation (PMM)", "Multiple imputation (full stochastic)")
  )

p <- se_summary %>%
  filter(term == "w_diff_c") %>%
  ggplot(aes(x = se_diff_percent, y = method, color = factor(sd_u_n))) +
  geom_point(position = position_dodge(0.8)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(forcats::fct_relevel(paste0("n = ", n)) |> fct_relevel("n = 500", "n = 1000") ~ forcats::fct_relevel(paste0("Proportion = ", calibration_p))) +
  labs(x = stringr::str_wrap("Relative difference: (mean standard error - empirical standard error)/empirical standard error", 40), 
       y = "") +
  scale_color_manual(values = c("#03244d", "#dd550c", "#496e9c"), name = stringr::str_wrap("Measurement error at follow-up (cm/s)", 10), breaks = c(150, 100, 50)) +
  theme_bw() +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_continuous(labels = scales::percent)

ggsave("../figs/11_se_bias_dotplot.pdf", p, width = 7, height = 4, units = "in")
