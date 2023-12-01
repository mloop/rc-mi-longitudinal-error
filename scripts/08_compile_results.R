library(tidyverse)

naive <- read_rds("../output/02_naive.rds") %>% bind_rows() %>% mutate(method = "naive")
complete_case <- read_rds("../output/03_complete_case.rds") %>% bind_rows() %>% mutate(method = "complete case")
snipe_boot <- read_rds("../output/04_02_snipe_boot.rds") %>% bind_rows() %>% mutate(method = "snipe (boot)")
multiple_imputation <- read_rds("../output/05_multiple_imputation.rds") %>% bind_rows() %>% mutate(method = "multiple imputation (pmm)")
multiple_imputation_stochastic <- read_rds("../output/05_multiple_imputation_full_stochastic.rds") %>% bind_rows() %>% mutate(method = "multiple imputation (full stochastic)")
true <- read_rds("../output/07_true.rds") %>% bind_rows() %>% mutate(method = "true", term = if_else(term == "x_diff_c", "w_diff_c", term))  # the true method is actually using the true values of x_diff_c and not w_diff_c. But, I want to make keep things consistent for the plotting code later 

results <- bind_rows(naive, complete_case, snipe_boot, multiple_imputation, multiple_imputation_stochastic, true) %>%
  ungroup()

write_rds(results, "../output/08_results.rds")
