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

p <- re_summary %>%
  filter(term == "w_diff_c") %>%
  ggplot(aes(x = relative_efficiency, y = method)) +
  geom_point() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_grid(device_bias ~ measurement_error)

ggsave("../figs/10_relative_efficiency_dotplot.pdf", p, width = 12, height = 6, units = "in")
