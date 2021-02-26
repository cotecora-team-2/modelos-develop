data {
  int<lower=0> n_1;
  int<lower=0> n_2;
  real media_params[2];
  real sd_params;
}

// The parameters accepted by the model.
parameters {

}

transformed parameters {

}

generated quantities {
  real mu[2];
  real<lower=0> sigma[2];
  real y_1_sim[n_1];
  real y_2_sim[n_2];

  for(j in 1:2){
    mu[j] = normal_rng(media_params[1], media_params[2]);
    sigma[j] = fabs(normal_rng(0, sd_params));
  }
  for(i in 1:n_1){
    y_1_sim[i] = normal_rng(mu[1], sigma[1]);
  }
    for(i in 1:n_2){
    y_2_sim[i] = normal_rng(mu[2], sigma[2]);
  }
  real dif = mu[1] - mu[2];
  real mayor_1 = mu[1] > mu[2];
}

