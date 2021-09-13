data {
  int<lower=0> n;
  vector[n] pwv_visit1_measured;
  vector[n] pwv_visit2_measured;
  vector[n] female;
  real<lower = 0, upper = 1> me_reduction;
  vector[n] age_centered;
  vector[n] brain_volume;
}


parameters {
  
  vector<lower=300, upper=2500>[n] pwv_visit2;
  vector<lower=300, upper=2500>[n] pwv_visit1;
  
  real<lower=0> beta_0;
  real beta_female;
  real beta_diff_c;
  real beta_age_c;
  
  real<lower=0> sigma;
  
  real<lower=300, upper=2500> mu_pwv_1;
  real<lower=0> sigma_pwv_1;
  real<lower=300, upper=2500> mu_pwv_2;
  real<lower=0> sigma_pwv_2;
}

transformed parameters {
  vector<lower=-2200,upper=2800>[n] diff;
  vector<lower=-2200, upper = 2800>[n] diff_c;
  
  diff = pwv_visit2 - pwv_visit1;
  diff_c = diff - mean(diff);
}

model {
  
  // likelihood
  pwv_visit1 ~ normal(mu_pwv_1, sigma_pwv_1);
  pwv_visit2 ~ normal(mu_pwv_2, sigma_pwv_2);
  pwv_visit1_measured ~ normal(pwv_visit1, 112.8);
  pwv_visit2_measured ~ normal(pwv_visit2, 112.8 * me_reduction);
  brain_volume ~ normal(beta_0 + beta_female * female + beta_diff_c * diff_c + beta_age_c * age_centered, sigma);
  
  // priors
  beta_0 ~ normal(1000, 200);
  sigma ~ student_t(50, 3, 10);
  beta_female ~ normal(0, 200);
  beta_diff_c ~ normal(0, 5);
  beta_age_c ~ normal(0, 5);
  mu_pwv_1 ~ normal(1100, 200);
  sigma_pwv_1 ~ student_t(1100, 300, 100);
  mu_pwv_2 ~ normal(1100, 200);
  sigma_pwv_2 ~ student_t(1100, 50, 100);
}
