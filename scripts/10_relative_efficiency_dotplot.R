library(readr)
library(data.table)
library(ggplot2)

results <- read_rds("../output/08_results.rds") |> setDT()

re_summary <- results[, .(empirical_standard_error = sd(estimate)), by = .(calibration_n, sd_u_n, term, method)
                      ][, empirical_standard_error := empirical_standard_error, keyby = .(calibration_n, sd_u_n, term, method)
                        ][, relative_efficiency := (1 / empirical_standard_error^2) / (1 / empirical_standard_error[.N]^2), by = .(calibration_n, sd_u_n, term)
                          ][.(method = c("snipe (sandwich)", "snipe (boot)", "naive", "multiple imputation (pmm)", "multiple imputation (full stochastic)", "complete case", "true"), to = c("Regression calibration (sandwich)", "Regression calibration (bootstrap)", "Naive", "Multiple imputation (PMM)", "Multiple imputation (full stochastic)", "Complete case", "Control")), on = "method", method := i.to
][, method := forcats::as_factor(method) |> forcats::fct_relevel("Naive", "Complete case", "Regression calibration (sandwich)", "Regression calibration (bootstrap)", "Multiple imputation (PMM)", "Multiple imputation (full stochastic)")][, calibration_n := forcats::as_factor(calibration_n) |> forcats::fct_relevel("50", "250")]

p <- re_summary[term == "w_diff_c", ] |>
  ggplot(aes(x = relative_efficiency, y = method, color = factor(sd_u_n))) +
  geom_point(position = position_dodge(0.8)) +
  labs(x = "Relative efficiency of estimate for association of interest", 
       y = "") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_wrap(~ forcats::fct_relevel(paste0("n = ", calibration_n), "n = 50", "n = 250"), ncol = 2) +
  theme_bw() +
  theme(
    plot.title.position = "plot"
  ) +
  scale_color_manual(values = c("#03244d", "#dd550c", "#496e9c"), name = stringr::str_wrap("Measurement error at follow-up (cm/s)", 10), breaks = c(150, 100, 50))

ggsave("../figs/10_relative_efficiency_dotplot.pdf", p, width = 7, height = 4, units = "in")
