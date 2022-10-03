library(tidyverse)
library(broom)

sims <- read_rds("../data/01_simulated_data.rds")

# Perform "calibration method" at visit 2, then use predicted values on new machine using data on old machine
fit <- sims %>%
  mutate(
    df  = map(df, ~ungroup(.x)),
    calib_fit = map(df, ~lm(w_f_o ~ w_f_n, data = filter(., sampled_for_calibration == 1))),  # Perform calibration study
    
    # To get the models and appropriate variables to talk to each other 'nicely', some book keeping with variable names has to be done. The w_b_o values are renamed to w_f_o temporarily in order to use them in the calib_fit models. Then, the names are immediately changed back. This is done now:
    #####
    df_calib = map2(df, calib_fit, ~modelr::add_predictions(.x, model = .y) %>%
                      
    ########
                      mutate(
                        w_diff = if_else(sampled_for_calibration == 0, pred, w_f_o) - w_b_o,  # use the predicted follow up values on the old machine if not in the calibration study; otherwise use the observed values
                        w_diff_c = scale(w_diff, scale = FALSE)  %>% as.numeric()
                      )
    ),
    fits = map(df_calib, ~lm(brain_volume ~ w_diff_c + female + age_centered, data = .) %>%
                 tidy()
               )
  ) %>%
  select(-calib_fit, -df, -df_calib) %>%
  unnest(fits)

fit %>% write_rds(file = "../output/04_snipe.rds")