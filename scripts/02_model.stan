
data {
  int<lower=0> n;
  vector[n] pwv_visit1_measured;
  vector[n] pwv_visit2_measured;
  vector[n] female;
}


parameters {
  vector<lower=300,upper=2500>[n] pwv_visit2;
  vector<lower=300,upper=2500>[n] pwv_visit1;
  real<lower=0> beta_0;
  real<lower=0> sigma;
  real beta;
  real beta_1;
  real<lower=0> mu_pwv_1;
  real<lower=0> sigma_pwv_1;
}

transformed parameters {
  vector<lower=-2200,upper=2800>[n] pwv_visit1_c;
  
  pwv_visit1_c = pwv_visit1 - mean(pwv_visit1);
}

model {
  
  // likelihood
  pwv_visit1 ~ normal(mu_pwv_1, sigma_pwv_1);
  pwv_visit1_measured ~ normal(pwv_visit1, 112.8);
  pwv_visit2_measured ~ normal(pwv_visit2, 22.4);
  pwv_visit2 ~ normal(beta_0 + beta * female + beta_1 * pwv_visit1_c, sigma);
  
  // priors
  beta_0 ~ normal(1120, 10);
  sigma ~ student_t(50, 3, 10);
  beta ~ normal(0, 5);
  beta_1 ~ normal(0, 5);
  mu_pwv_1 ~ normal(1100, 200);
  sigma_pwv_1 ~ student_t(1100, 300, 100);
}
