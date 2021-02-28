data {
  //int N; // number of stations
  //int<lower=0> y[N]; // observed vote counts
  //vector<lower=0>[N] n; // nominal counts
  //int stratum[N];
  //matrix[N, n_covariates] x;

  int N_f; // total number of stations
  int n_strata_f;
  int n_covariates_f;
  int<lower=0> y_f[N_f] ; // observed vote counts
  vector[N_f] in_sample;
  vector[N_f] n_f; // nominal counts
  int stratum_f[N_f];
  matrix[N_f, n_covariates_f] x_f;

  // conf
  vector[2] beta_0_param;
  real sigma_param;
  vector[2] beta_bn_param;

}

parameters {

}

transformed parameters {

}

model {

}

generated quantities {
  int y_out;
  real pred_f;
  // sim parameters
  vector[n_covariates_f] beta;
  real beta_0;
  real<lower=0> sigma;
  vector[n_strata_f] beta_bn_f;

  int y_f_sim[N_f];

  vector[n_strata_f] beta_st_raw;
  vector[n_strata_f] beta_st;

  sigma = fabs(normal_rng(0, sigma_param));
  beta_0 = normal_rng(beta_0_param[1], beta_0_param[2]);

  for(i in 1:n_covariates_f){
    beta[i] = normal_rng(0, 1);
  }
  for(j in 1:n_strata_f){
    beta_st_raw[j] = normal_rng(0, 1);
    beta_st[j] = beta_0 + beta_st_raw[j] * sigma;
    beta_bn_f[j] = gamma_rng(beta_bn_param[1], beta_bn_param[2]);
  }
  // simulate counts
  y_out = 0;
  for(i in 1:N_f){
    vector[N_f] alpha_bn_f;
    real theta_f;
    if(in_sample[i]==1){
      y_out += y_f[i];
    } else {
      pred_f = dot_product(x_f[i,], beta);
      theta_f = inv_logit(beta_st[stratum_f[i]] + pred_f);
      alpha_bn_f[i] =  n_f[i] * theta_f;
      y_f_sim[i] = neg_binomial_2_rng(alpha_bn_f[i] , beta_bn_f[stratum_f[i]]*alpha_bn_f[i]);
      y_out += y_f_sim[i];
    }
  }
}

