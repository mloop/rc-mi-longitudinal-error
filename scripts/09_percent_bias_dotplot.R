library(tidyverse)

results <- read_rds("../output/08_results.rds")
dir.create("../figs/", showWarnings = FALSE)
bias_summary <- results %>%
  group_by(mu_u_n, sd_u_n, term, method) %>%
  mutate(
    true_value = case_when(
      term == "(Intercept)" ~ 1000,
      term == "age_centered" ~ -4.267,
      term == "female" ~ -125.217,
      term == "w_diff_c" ~ -0.2
    )
  ) %>%
  summarise(
    bias = mean(estimate) - true_value
  ) %>%
  mutate(
    true_value = case_when(
      term == "(Intercept)" ~ 1000,
      term == "age_centered" ~ -4.267,
      term == "female" ~ -125.217,
      term == "w_diff_c" ~ -0.2
    ), 
    percent_bias = bias / true_value * 100
  ) %>%
  ungroup() %>%
  mutate(
    method = factor(method) %>% fct_recode("Regression calibration (sandwich)" = "snipe (sandwich)", 
                                           "Naive" = "naive", 
                                           "Multiple imputation" = "multiple imputation", 
                                           "Complete case" = "complete case", 
                                           "True" = "true") %>%
fct_relevel("Naive", "Complete case", "Regression calibration (sandwich)", "Multiple imputation", "True")
)

p <- bias_summary %>%
  filter(term == "w_diff_c") %>%
  ggplot(aes(x = percent_bias, y = method, color = factor(sd_u_n))) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Percent bias for association of interest", 
       y = "", 
       title = "Percent bias of each method as measurement error at follow-up changes") +
  scale_color_manual(values = c("#03244d", "#dd550c", "#496e9c"), name = str_wrap("Measurement error at follow-up (cm/s)", 10)) +
  theme(
    plot.title.position = "plot"
  )

ggsave("../figs/09_percent_bias_dotplot.pdf", p, width = 7, height = 4, units = "in")

