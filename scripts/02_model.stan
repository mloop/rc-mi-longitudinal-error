
data {
  int<lower=0> n;
  vector[n] pwv_visit1_measured;
  vector[n] pwv_visit2_measured;
  vector[n] female;
}


parameters {
  vector[n] pwv_visit2;
  vector[n] pwv_visit1;
  real mu_u;
  real<lower=0> sigma;
  real beta;
  real beta_1;
  real mu_pwv_1;
  real<lower=0> sigma_pwv_1;
}

model {
  
  // likelihood
  pwv_visit1 ~ normal(mu_pwv_1, sigma_pwv_1);
  pwv_visit1_measured ~ normal(pwv_visit1, 112.8);
  pwv_visit2_measured ~ normal(pwv_visit2, 22.4);
  pwv_visit2 ~ normal(mu_u + beta * female + beta_1 * pwv_visit1, sigma);
  
  // priors
  mu_u ~ normal(1100, 350);
  sigma ~ student_t(50, 3, 10);
  beta ~ normal(0, 1);
  beta_1 ~ normal(0, 1);
  mu_pwv_1 ~ normal(1100, 100);
  sigma_pwv_1 ~ student_t(1100, 300, 100);
}
