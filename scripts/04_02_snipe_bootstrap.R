library(tidyverse)
library(broom)
library(boot)

sims <- read_rds("../data/01_simulated_data.rds")

boot_rc <- function(data, index){
  d <- data[index, ]
  
  c <- filter(d, sampled_for_calibration == 1)
  
  c_fit <- lm(x_f ~ w_f_n + female + age_centered + x_b,
              data = c)
  
  df_calib <- modelr::add_predictions(d, model = c_fit) %>%
    mutate(
      w_diff = pred - x_b,
      w_diff_c = scale(w_diff, scale = FALSE)  %>% as.numeric()
    )
  
  m <- lm(brain_volume ~ w_diff_c + female + age_centered, data = df_calib)
  
  est <- coef(m)["w_diff_c"] %>% as.numeric()
  
  return(est)
  
}

# Perform "calibration method" at visit 2, then use predicted values on new machine using data on old machine
fit <- sims %>%
  mutate(
    df  = map(df, ~ungroup(.x)),
    calib_fit = map(df, ~lm(x_f ~ w_f_n + female + age_centered + x_b, data = filter(., sampled_for_calibration == 1))),  # Perform calibration study
    
    #####
    df_calib = map2(df, calib_fit, ~modelr::add_predictions(.x, model = .y) %>%
                      
                      ########
                    mutate(
                      w_diff = pred - x_b,
                      w_diff_c = scale(w_diff, scale = FALSE)  %>% as.numeric()
                    )
    ),
    fits_initial = map(df_calib, ~lm(brain_volume ~ w_diff_c + female + age_centered, data = .)),
    
    se_boot = map(df_calib, ~boot(data = .x, statistic = boot_rc, R = 200)$t %>%
                        sd() %>%
      tibble(std.error = ., term = "w_diff_c")),
    tidy_fits = map(fits_initial, ~tidy(.) %>%
                      select(term, estimate)),
    
    fits = map2(tidy_fits, se_boot, ~left_join(.x, .y, by = "term"))
    
  ) %>%
  select(-calib_fit, -df, -df_calib, -fits_initial, -se_boot, -tidy_fits, -data) %>%
  unnest(fits)

fit %>% write_rds(file = "../output/04_02_snipe_boot.rds")