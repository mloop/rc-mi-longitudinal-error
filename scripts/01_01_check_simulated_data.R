library(tidyverse)

sims <- read_rds("../data/01_simulated_data.rds") %>%
  mutate(
    summaries = map(df, ~ungroup(.) %>%
      select(pwv_visit1, pwv_visit2) %>%
      pivot_longer(cols = everything(), names_to = "measurement", values_to = "value") %>%
      group_by(measurement) %>%
      summarise(
        mean = mean(value),
        sd = sd(value),
        min = min(value),
        max = max(value)
      )
      )
) %>%
  unnest(summaries) %>%
  select(-df) 

sims %>%
  pivot_longer(cols = mean:max, names_to = "summary", values_to = "value") %>%
  ggplot(aes(x = value, y = summary, color = measurement)) +
  geom_point(position = position_dodge(0.8))
