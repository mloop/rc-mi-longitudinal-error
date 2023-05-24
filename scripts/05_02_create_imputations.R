library(tidyverse)
library(broom)
library(mice)


for(i in 1:1000){
  
  set.seed(987234+i)

  x <- read_rds(paste0("../output/05_01_", i, ".rds"))

  fit_imp <- x %>%
    mutate(
      imp = map(df, ~mutate(., x_f_c = if_else(sampled_for_calibration == 1, x_f, NA_real_),  # x_f_c just is a renaming of the variable x_f, which we are setting to missing for those who weren't in the calibration study. It's the same as x_f for people who *were* in the calibration study.
                            w_diff = NA) %>%
                  select(x_b, w_f_n, x_f_c, age_centered, female, brain_volume) %>%
                  mice(m = 100, 
                       printFlag = FALSE) %>%
                  mice::complete(action = "long", include = TRUE) %>% 
                  as_tibble()
      ),
      
      modified_imp = map(imp, ~mutate(., w_diff = x_f_c - x_b) %>%
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
