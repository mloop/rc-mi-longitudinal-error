library(tidyverse)

results <- read_rds("../output/08_results.rds")

se_summary <- results %>%
  group_by(mu_u_o, mu_u_n, sd_u_o, sd_u_n, term, method) %>%
  summarise(
    empirical_standard_error = sd(estimate),
    avg_se = mean(std.error),
    se_diff = avg_se - empirical_standard_error,
    se_diff_percent = se_diff / empirical_standard_error * 100
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

p <- se_summary %>%
  filter(term == "w_diff_c") %>%
  ggplot(aes(x = se_diff_percent, y = method)) +
  geom_point() +
  facet_grid(device_bias ~ measurement_error) +
  ggsci::scale_color_aaas()

ggsave("../figs/11_percent_re_dotplot.pdf", p, width = 12, height = 6, units = "in")
