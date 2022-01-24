data {
  int<lower=0> n;
  int<lower=0> j;
  vector[n] w_b_o;
  vector[n] w_f_n;
  vector[n] w_f_o;
  vector[n] female;
  vector[n] age_centered;
  vector[n] brain_volume;
  int calibration_indices[j];
}


parameters {
  
  vector<lower=300, upper=2500>[n] x_f;
  vector<lower=300, upper=2500>[n] x_b;
  
  real<lower=0> beta_0;
  real beta_female;
  real beta_diff_c;
  real beta_age_c;
  
  real<lower=0> sigma;
  
  real<lower=300, upper=2500> mu_x_b;
  real<lower=0> sigma_x_b;
  real<lower=300, upper=2500> mu_x_f;
  real<lower=0> sigma_x_f;
  
  real mu_u_b;
  real mu_u_f;
  
  real alpha_0;
  real alpha_1;
  real<lower=0> sigma_calibration;
  real<lower=0> sigma_prediction;
}

transformed parameters {
  vector<lower=-2200,upper=2800>[n] diff;
  vector<lower=-2200, upper = 2800>[n] diff_c;
  vector[n] predicted_w_b_n;
  
  diff = x_f - x_b;
  diff_c = diff - mean(diff);
  
  predicted_w_b_n = alpha_0 + alpha_1 * w_b_o;
}

model {
  // calibration study
    w_f_n[calibration_indices] ~ normal(alpha_0 + alpha_1 * w_f_o[calibration_indices], sigma_calibration);
  
  // likelihood
  x_b ~ normal(mu_x_b, sigma_x_b);
  x_f ~ normal(mu_x_f, sigma_x_f);
  predicted_w_b_n ~ normal(x_b + mu_u_b, sigma_prediction); // hope/assumption is that predicted_w_b_n is an unbiased estimator of x_b + mu_u_f, such that the biases in delta(w) cancel out and delta(w) is an unbiased estimator of delta(x)
  target += alpha_1;
  
  w_b_o ~ normal(x_b + mu_u_b, 112.8);
  w_f_n ~ normal(x_f + mu_u_f, 112.8);
  
  brain_volume ~ normal(beta_0 + beta_female * female + beta_diff_c * diff_c + beta_age_c * age_centered, sigma);
  
  // priors
  beta_0 ~ normal(1000, 200);
  sigma ~ student_t(50, 3, 10);
  beta_female ~ normal(0, 200);
  beta_diff_c ~ normal(0, 5);
  beta_age_c ~ normal(0, 5);
  mu_x_b ~ normal(1100, 200);
  sigma_x_b ~ student_t(1100, 300, 100);
  mu_x_f ~ normal(1100, 200);
  sigma_x_f ~ student_t(1100, 300, 100);
  mu_u_b ~ normal(0, 500);
  mu_u_f ~ normal(0, 500);
  alpha_0 ~ normal(0, 1000);
  alpha_1 ~ normal(0, 200);
  sigma_calibration ~ student_t(1100, 50, 100);
  sigma_prediction ~ student_t(1100, 50, 100);
}
