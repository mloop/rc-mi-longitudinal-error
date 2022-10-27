library(tidyverse)

naive <- read_rds("../output/02_naive.rds") %>% mutate(method = "naive")
complete_case <- read_rds("../output/03_complete_case.rds") %>% mutate(method = "complete case")
snipe <- read_rds("../output/04_snipe.rds") %>% mutate(method = "snipe (bootstrap)")
snipe_s <- read_rds("../output/04_01_snipe_sandwich.rds") %>% mutate(method = "snipe (sandwich)")
multiple_imputation <- read_rds("../output/05_multiple_imputation.rds") %>% mutate(method = "multiple imputation")
true <- read_rds("../output/07_true_model.rds") %>% mutate(method = "true", term = if_else(term == "x_diff_c", "w_diff_c", term))  # the true method is actually using the true values of x_diff_c and not w_diff_c. But, I want to make keep things consistent for the plotting code later 

results <- bind_rows(naive, complete_case, snipe, multiple_imputation, true) %>%
  ungroup()

write_rds(results, "../output/08_results.rds")
