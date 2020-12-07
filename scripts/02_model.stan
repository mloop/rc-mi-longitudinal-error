//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> n;
  //vector[n] pwv_visit1_measured ;
  //vector[n] pwv_visit2_measured;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
//parameters {
//  vector[n] pwv_visit1;
//  vector[n] pwv_visit2;
  //real mu;
  //real<lower=0> sigma;
//}

//transformed parameters {
  //vector[n] z_diff;
  //z_diff = pwv_visit2 - pwv_visit2;
//}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
//model {
  
  // likelihood
  //pwv_visit1_measured ~ normal(pwv_visit1, 20);
  //pwv_visit2_measured ~ normal(pwv_visit2, 10);
  //z_diff ~ normal(mu, sigma);
  
  // priors
  //mu ~ normal(20, 10);
  //sigma ~ normal(0, 1);
//}

generated quantities {
  vector[n] diff;
  vector[n] pwv_visit1 = normal_rng(1100, 350);
  vector pwv_visit2[n] = normal_rng(1130, 350);
  
  vector pwv_visit1_measured[n] = normal_rng(pwv_visit1, 20);
  vector pwv_visit2_measured[n] = normal_rng(pwv_visit2, 10);
  diff[n] = pwv_visit2_measured - pwv_visit1_measured;
}
