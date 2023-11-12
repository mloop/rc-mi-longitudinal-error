library(readr)
library(data.table)
library(ggplot2)

results <- read_rds("../output/08_results.rds") |> setDT()
dir.create("../figs/", showWarnings = FALSE)
bias_summary <- results[term == "(Intercept)", true_value := 1000
                        ][term == "age_centered", true_value := -4.267
                          ][term == "female", true_value := -125.217
                            ][term == "w_diff_c", true_value := -0.2
                              ][, .(bias = mean(estimate - true_value), percent_bias = mean(estimate - true_value) / mean(true_value)), by = .(n, calibration_p, sd_u_n, term, method)
                                ][.(method = c("snipe (boot)", "naive", "multiple imputation (pmm)", "multiple imputation (full stochastic)", "complete case", "true"), to = c( "Regression calibration (bootstrap)", "Naive", "Multiple imputation (PMM)", "Multiple imputation (full stochastic)", "Complete case", "Control")), on = "method", method := i.to
                                  ][, method := forcats::as_factor(method) |> forcats::fct_relevel("Naive", "Complete case", "Regression calibration (bootstrap)", "Multiple imputation (PMM)", "Multiple imputation (full stochastic)")]

p <- bias_summary[term == "w_diff_c" & method != "Regression calibration (sandwich)", ] |>
  ggplot(aes(x = percent_bias, y = method, color = factor(sd_u_n))) +
  geom_point(position = position_dodge(0.8)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(forcats::fct_relevel(paste0("n = ", n)) |> fct_relevel("n = 500", "n = 1000") ~ forcats::fct_relevel(paste0("Proportion = ", calibration_p))) +
  labs(x = "Percent bias for association of interest", 
       y = "") +
  scale_color_manual(values = c("#03244d", "#dd550c", "#496e9c"), name = stringr::str_wrap("Measurement error at follow-up (cm/s)", 10), breaks = c(150, 100, 50)) +
  theme_bw() +
  theme(
    plot.title.position = "plot"
  ) +
  scale_x_continuous(labels = scales::percent, limits = c(-0.12, 0.05))

ggsave("../figs/09_percent_bias_dotplot.pdf", p, width = 7, height = 4, units = "in")

