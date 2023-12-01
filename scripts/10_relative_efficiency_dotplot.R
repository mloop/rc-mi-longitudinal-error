library(tidyverse)

results <- read_rds("../output/08_results.rds")

re_summary <- results %>%
  ungroup() %>%
  group_by(n, calibration_p, sd_u_n, term, method) %>%
  summarise(
    empirical_standard_error = sd(estimate)
  ) %>%
  ungroup() %>%
  group_by(n, calibration_p, sd_u_n, term) %>%
  mutate(
    relative_efficiency = (1 / empirical_standard_error^2) / (1 / last(empirical_standard_error)^2)
  ) %>%
  mutate(
    method = factor(method) %>%
      fct_recode("Regression calibration (bootstrap)" = "snipe (boot)",
                 "Naive" = "naive",
                 "Multiple imputation (PMM)" = "multiple imputation (pmm)",
                 "Multiple imputation (full stochastic)" = "multiple imputation (full stochastic)",
                 "Complete case" = "complete case", 
                 "Control" = "true") %>%
      fct_relevel("Naive", "Complete case", "Regression calibration (bootstrap)", "Multiple imputation (PMM)", "Multiple imputation (full stochastic)")
  )

p <- re_summary %>%
  filter(term == "w_diff_c") %>%
  ggplot(aes(x = relative_efficiency, y = method, color = factor(sd_u_n))) +
  geom_point(position = position_dodge(0.8)) +
  labs(x = "Relative efficiency of estimate for association of interest", 
       y = "") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_grid(forcats::fct_relevel(paste0("n = ", n)) |> fct_relevel("n = 500", "n = 1000") ~ forcats::fct_relevel(paste0("Proportion = ", calibration_p))) +
  theme_bw() +
  theme(
    plot.title.position = "plot"
  ) +
  scale_color_manual(values = c("#03244d", "#dd550c", "#496e9c"), name = stringr::str_wrap("Measurement error at follow-up (cm/s)", 10), breaks = c(150, 100, 50))

ggsave("../figs/10_relative_efficiency_dotplot.pdf", p, width = 7, height = 4, units = "in")
