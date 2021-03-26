data {

  // population
  int N_f; // total number of stations
  int n_strata_f;
  int n_covariates_f;
  int p; // number of parties
  int<lower=0> y_f[N_f, p] ; // observed vote counts
  int in_sample[N_f];
  vector[N_f] n_f; // nominal counts
  int stratum_f[N_f];
  matrix[N_f, n_covariates_f] x_f;


  // sample
  int N; // number of stations
  int<lower=0> y[N, p]; // observed vote counts
  vector<lower=0>[N] n; // nominal counts
  int stratum[N];
  matrix[N, n_covariates_f] x;
  real<lower=0> p_obs;

  // conf
  vector[2] beta_0_param;
  real sigma_param;
  vector[2] kappa_param;
  real sigma_coefs;
  real f_bias;
}

transformed data {
  int<lower=0> total_f[N_f] ; // observed totals
  int<lower=0> total[N] ; // observed totals

  for(i in 1:N_f){
    total_f[i] = sum(y_f[i, ]);
  }
  for(i in 1:N){
    total[i] = sum(y[i, ]);
  }

}

parameters {
  real beta_0[p+1];
  matrix[n_covariates_f, p+1] beta;
  matrix<lower=0>[n_strata_f, p+1] kappa;
  real<lower=0> sigma[p+1];
  matrix[n_strata_f,p+1] beta_st_raw;
}

transformed parameters {
   matrix<lower=0, upper=1>[N,p+1] theta;
   matrix<lower=0>[N,p+1] alpha_bn;
   matrix[n_strata_f,p+1] beta_st;
   matrix[N,p+1] pred;

   beta_st[,p+1] = beta_0[p+1] + beta_st_raw [,p+1]* sigma[p+1];
   pred[,p+1] = x * beta[, p+1];
   theta[,p+1] = inv_logit(beta_st[stratum, p+1] + pred[,p+1]);
   alpha_bn[,p+1] = n .* theta[,p+1] ;


  for(k in 1:p){
   beta_st[,k] = beta_0[k] + beta_st_raw [,k]* sigma[k];
   pred[,k] = x * beta[, k];
   theta[,k] = inv_logit(beta_st[stratum, k] + pred[,k]);
   alpha_bn[,k] =(n .* theta[,k]) .* theta[,p+1]; // mult by part rate
  }

}

model {

  beta_0 ~ normal(beta_0_param[1], beta_0_param[2]);
  for(k in 1:(p + 1)){
    beta[,k] ~ normal(0 , sigma_coefs);
    beta_st_raw[,k] ~ normal(0, 1);
    kappa[,k] ~ gamma(kappa_param[1], kappa_param[2]);
  }
  sigma ~ normal(0, sigma_param);

  for(k in 1:p){
    y[,k] ~ neg_binomial_2( alpha_bn[, k], alpha_bn[, k] ./ kappa[stratum,k]);
  }
  total ~ neg_binomial_2( alpha_bn[ , p + 1], alpha_bn[, p+1]  ./ kappa[stratum,p+1]);
}

generated quantities {
  real y_out[p];
  real prop_votos[p];
  real theta_f;
  real alpha_bn_f;
  real pred_f;
  real theta_f_total[N_f];
  real total_cnt;
  real w_bias;
  real participacion;
  real total_est[N_f];

  // total
  w_bias = normal_rng(0, (1 - p_obs) / f_bias);
  total_cnt = 0;
  for(i in 1:N_f){
      if(in_sample[i] == 1){
        total_est[i] = total_f[i];
        total_cnt += total_f[i];
      } else {
        pred_f = dot_product(x_f[i,], beta[,p+1]);
        theta_f_total[i] = inv_logit(beta_st[stratum_f[i],p+1] + pred_f + w_bias);
        alpha_bn_f =  n_f[i] * theta_f_total[i];
        total_est[i] = neg_binomial_2_rng(alpha_bn_f , alpha_bn_f/kappa[stratum_f[i],p+1]);
        total_cnt += total_est[i];
      }
    }

// party vote
 for(k in 1:p){
    w_bias = normal_rng(0, (1 - p_obs) / f_bias);
    y_out[k] = 0;
    for(i in 1:N_f){
      if(in_sample[i] == 1){
        y_out[k] += y_f[i,k];
      } else {
        pred_f = dot_product(x_f[i,], beta[,k]);
        theta_f = inv_logit(beta_st[stratum_f[i],k] + pred_f + w_bias);
        //alpha_bn_f =  n_f[i] * theta_f * theta_f_total[i];
        //y_out[k] += neg_binomial_2_rng(alpha_bn_f , alpha_bn_f/kappa[stratum_f[i],k]);
        y_out[k] = y_out[k] + total_est[i] * theta_f;
      }
    }
  }
  for(k in 1:p){
    prop_votos[k] = y_out[k] / total_cnt;
  }
  participacion = total_cnt / sum(n_f);
}
