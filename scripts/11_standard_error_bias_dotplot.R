library(readr)
library(data.table)
library(ggplot2)

results <- read_rds("../output/08_results.rds") |> setDT()

se_summary <- results[, .(empirical_standard_error = sd(estimate),
                          avg_se = mean(std.error)), by = .(n, calibration_p, sd_u_n, term, method)
][,
  `:=` (se_diff = avg_se - empirical_standard_error,
        se_diff_percent = (avg_se - empirical_standard_error) / empirical_standard_error)
][.(method = c("snipe (boot)", "naive", "multiple imputation (pmm)", "multiple imputation (full stochastic)", "complete case", "true"), to = c("Regression calibration (bootstrap)", "Naive", "Multiple imputation (PMM)", "Multiple imputation (full stochastic)", "Complete case", "Control")), on = "method", method := i.to
][, method := forcats::as_factor(method) |> forcats::fct_relevel("Naive", "Complete case", "Regression calibration (bootstrap)", "Multiple imputation (PMM)", "Multiple imputation (full stochastic)")]

p <- se_summary[term == "w_diff_c" & method != "Regression calibration (sandwich)"] |>
  ggplot(aes(x = se_diff_percent, y = method, color = factor(sd_u_n))) +
  geom_point(position = position_dodge(0.8)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(forcats::fct_relevel(paste0("n = ", n)) |> fct_relevel("n = 500", "n = 1000") ~ forcats::fct_relevel(paste0("Proportion = ", calibration_p))) +
  labs(x = stringr::str_wrap("Relative difference: (mean standard error - empirical standard error)/empirical standard error", 40), 
       y = "") +
  scale_color_manual(values = c("#03244d", "#dd550c", "#496e9c"), name = stringr::str_wrap("Measurement error at follow-up (cm/s)", 10), breaks = c(150, 100, 50)) +
  theme_bw() +
  theme(
    plot.title.position = "plot"
  ) +
  scale_x_continuous(labels = scales::percent)

ggsave("../figs/11_se_bias_dotplot.pdf", p, width = 7, height = 4, units = "in")
