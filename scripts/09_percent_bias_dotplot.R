library(tidyverse)

results <- read_rds("../output/08_results.rds")
dir.create("../figs/", showWarnings = FALSE)
bias_summary <- results %>%
  group_by(mu_u_o, mu_u_n, sd_u_o, sd_u_n, term, method) %>%
  mutate(
    true_value = case_when(
      term == "(Intercept)" ~ 1000,
      term == "age_centered" ~ -4.267,
      term == "female" ~ -125.217,
      term == "w_diff_c" ~ -0.2
    )
  ) %>%
  summarise(
    bias = mean(estimate - true_value),
    percent_bias = bias / true_value * 100
  ) %>%
  ungroup() %>%
  mutate(
    device_bias = case_when(
      mu_u_n == mu_u_o ~ "equal (10 cm/s)",
      mu_u_n > mu_u_o ~ "new 5 cm/s more",
      mu_u_n < mu_u_o ~ "new 5 cm/s less"
    ) %>% factor() %>% fct_relevel("new 5 cm/s less", "equal (10 cm/s)"),
    measurement_error = case_when(
      sd_u_n == sd_u_o ~ "equal (113 cm/s)",
      sd_u_n > sd_u_o ~ "new 113 cm/s, old 50 cm/s",
      sd_u_n < sd_u_o ~ "new 50 cm/s, old 113 cm/s"
    ) %>% factor() %>% fct_relevel("new 50 cm/s, old 113 cm/s", "equal (113 cm/s)")
  )

p <- bias_summary %>%
  filter(term == "w_diff_c") %>%
  ggplot(aes(x = percent_bias, y = method)) +
  geom_point() +
  facet_grid(device_bias ~ measurement_error)

ggsave("../figs/09_percent_bias_dotplot.pdf", p, width = 12, height = 6, units = "in")
