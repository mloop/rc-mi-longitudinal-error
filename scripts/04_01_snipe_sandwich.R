library(tidyverse)
library(broom)
library(sandwich)

sims <- read_rds("../data/01_simulated_data.rds")

# Perform "calibration method" at visit 2, then use predicted values on new machine using data on old machine
fit <- sims %>%
  mutate(
    df  = map(df, ~ungroup(.x)),
    calib_fit = map(df, ~lm(w_f_o ~ w_f_n, data = filter(., sampled_for_calibration == 1))),  # Perform calibration study
    
    #####
    df_calib = map2(df, calib_fit, ~modelr::add_predictions(.x, model = .y) %>%
                      
                      ########
                    mutate(
                      w_diff = if_else(sampled_for_calibration == 0, pred, w_f_o) - w_b_o,  # use the predicted follow up values on the old machine if not in the calibration study; otherwise use the observed values
                      w_diff_c = scale(w_diff, scale = FALSE)  %>% as.numeric()
                    )
    ),
    fits_initial = map(df_calib, ~lm(brain_volume ~ w_diff_c + female + age_centered, data = .)),
    
    se_sandwich = map(fits_initial, ~sandwich(.) %>%
                          tibble(term = rownames(.), std.error = sqrt(diag(.))) %>%
                          select(2:3)),
    tidy_fits = map(fits_initial, ~tidy(.) %>%
                      select(term, estimate)),
    
    fits = map2(tidy_fits, se_sandwich, ~left_join(.x, .y, by = "term"))
    
  ) %>%
  select(-calib_fit, -df, -df_calib, -fits_initial, -se_sandwich, -tidy_fits, -data) %>%
  unnest(fits)

fit %>% write_rds(file = "../output/04_01_snipe_sandwich.rds")