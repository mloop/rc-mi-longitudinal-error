library(tidyverse)
library(ggsci)

fits_true <- read_rds("../output/02_simple_lm.rds")
fits_obs <- read_rds("../output/02_simple_lm_observed.rds")

bias <- bind_rows(fits_true %>% select(iteration, fits) %>% mutate(method = "true"),
                  fits_obs %>% select(iteration, fits) %>% mutate(method = "observed")) %>%
  mutate(
    bhat_int = map_dbl(fits, ~coefficients(.)[1]),
    bhat_pwv = map_dbl(fits, ~coefficients(.)[2]),
    bhat_female = map_dbl(fits, ~coefficients(.)[3]),
    hat_sigma = map_dbl(fits, ~sigma(.))
  ) %>%
  select(-fits) %>%
  pivot_longer(cols = contains("hat"),
               names_to = "term",
               values_to = "estimate") %>%
  ungroup() %>%
  group_by(method, term) %>%
  summarise(mean_estimate = mean(estimate)) %>%
  mutate(
    true_value = case_when(
        term == "bhat_int" ~ 20,
        term == "bhat_pwv" ~ 1,
        term == "bhat_female" ~ -5,
        term == "hat_sigma" ~ 10
      ),
    bias = mean_estimate - true_value,
    percent_bias = (mean_estimate - true_value) / true_value * 100
)

p_bias <- bias %>%
  mutate(term = factor(term) %>% 
           fct_recode("Residual standard deviation" = "hat_sigma", "Intercept" = "bhat_int", "Female" = "bhat_female", "PWV at visit 1" = "bhat_pwv") %>% 
           fct_reorder(percent_bias),
         method = factor(method) %>% fct_recode("True value" = "true",
                                                "Observed value" = "observed")) %>%
  ggplot(aes(x = percent_bias, y = term, color = method)) +
  geom_point(position = position_dodge(0.3)) +
  scale_color_nejm(name = "") +
  theme_classic() +
  labs(
    x = "Percent bias (%)",
    y = "",
    title = "Percent bias for each regression parameter and\nresidual standard deviation, by data analyzed"
  ) +
  theme(
    plot.title.position = "plot"
  )

ggsave(filename = "../figs/03_bias_plot.png", p_bias)
