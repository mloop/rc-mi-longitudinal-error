library(tidyverse)
library(broom)
library(mice)

i = commandArgs(trailingOnly=TRUE)

for(i in 1:1000){
  
  set.seed(987234+i)

  x <- read_rds(paste0("../output/05_01_", i, ".rds"))

  fit_imp <- x %>%
    mutate(
      imp = map(df, ~mutate(., w_f_o = if_else(sampled_for_calibration == 1, w_f_o, NA_real_),
                            w_diff = NA) %>%
                  select(w_b_o, w_f_n, w_f_o, age_centered, female, brain_volume) %>%
                  mice(m = 50, 
                       printFlag = FALSE) %>%
                  mice::complete(action = "long", include = TRUE) %>% 
                  as_tibble()
      ),
      
      modified_imp = map(imp, ~mutate(., w_diff = w_f_o - w_b_o) %>%
                           group_by(.imp) %>%
                           mutate(
                             w_diff_c = scale(w_diff, scale = FALSE) %>% as.numeric()
                           )
      ),
      
      new_mids = map(modified_imp, ~as.mids(.)),
      fits = map(new_mids, ~with(., lm(brain_volume ~ w_diff_c + 
                                         female + age_centered)) %>%
                   pool() %>%
                   tidy()
      )
    ) %>%
    select(-imp, -modified_imp, -df, -new_mids) %>%
    unnest(fits)
  
  write_rds(fit_imp, paste0("../output/05_02_", i, ".rds"))
}
