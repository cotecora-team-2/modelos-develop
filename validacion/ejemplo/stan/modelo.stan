data {
  int<lower=0> n_1;
  int<lower=0> n_2;
  vector[n_1] y_1;
  vector[n_2] y_2;
  real media_params[2];
  real sd_params;
}

// The parameters accepted by the model.
parameters {
  real mu[2];
  real<lower=0> sigma[2];
}

transformed parameters {
  real dif;
  real mayor_1;

  dif = mu[1] - mu[2];
  mayor_1 = mu[1] > mu[2];
}

// The model to be estimated.
model {
  y_1 ~ normal(mu[1], 2.0 + sigma[1]);
  y_2 ~ normal(mu[2], 2.0 + sigma[2]);
  mu ~ normal(media_params[1], media_params[2]);
  sigma ~ normal(0, sd_params);
}

generated quantities {

}
