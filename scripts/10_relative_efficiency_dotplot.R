library(tidyverse)

results <- read_rds("../output/08_results.rds")

re_summary <- results %>%
  group_by(mu_u_o, mu_u_n, sd_u_o, sd_u_n, term, method) %>%
  summarise(
    empirical_standard_error = sd(estimate)
  ) %>%
  mutate(relative_efficiency = (1 / empirical_standard_error ^ 2) / (1 / last(empirical_standard_error) ^ 2)) %>%
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

p <- re_summary %>%
  filter(term == "w_diff_c") %>%
  ggplot(aes(x = relative_efficiency, y = method)) +
  geom_point() +
  labs(x = "Relative efficiency of estimate for association of interest", 
       y = "Method", 
       title = "Relative efficiency of each method as measurement error and bias of each device changes") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_grid(device_bias ~ measurement_error,
             labeller = labeller(device_bias = label_wrap_gen(width = 25),
                                 measurement_error = label_wrap_gen(width = 25)))

ggsave("../figs/10_relative_efficiency_dotplot.pdf", p, width = 12, height = 6, units = "in")
