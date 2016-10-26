data {
  int<lower=0> N;
  int<lower=0> n_genres;

  vector[N] previous_plays;
  vector[N] plays;
  vector[N] danceability;
  int genre_index[N];
}
parameters {
  vector <lower=0, upper=20>[n_genres] beta0;
  real <lower=0,upper=20> mu_beta0;
  real <lower=-5,upper=5> sigma_beta0;

  vector <lower=-50,upper=50>[n_genres] beta1;
  real <lower=-50,upper=50> mu_beta1;
  real <lower=-50,upper=50> sigma_beta1;

  vector <lower=-20,upper=20>[n_genres] beta2;
  real <lower=-20,upper=20> mu_beta2;
  real <lower=-20,upper=20> sigma_beta2;

  vector <lower=0,upper=5>[n_genres] beta3;
  real <lower=-5,upper=5> mu_beta3;
  real <lower=0,upper=5> sigma_beta3;

  vector <lower=0,upper=5>[n_genres] beta4;
  real <lower=0,upper=5> mu_beta4;
  real <lower=0,upper=5> sigma_beta4;
}
transformed parameters {
  vector[N] linear_term;
  vector[N] shape;
  for (i in 1:N)
    linear_term[i] = beta0[genre_index[i]] + beta1[genre_index[i]] * danceability[i]
      + beta2[genre_index[i]] * danceability[i] * danceability[i] + beta4[genre_index[i]] * log(previous_plays[i]);
  for (i in 1:N)
    shape[i] = beta3[genre_index[i]];
}
model {
  beta0 ~ normal(mu_beta0, sigma_beta0);
  beta1 ~ normal(mu_beta1, sigma_beta1);
  beta2 ~ normal(mu_beta2, sigma_beta2);
  beta3 ~ lognormal(mu_beta3, sigma_beta3);
  beta4 ~ normal(mu_beta4, sigma_beta4);

  for (i in 1:N)
    plays[i] ~ gamma(shape[i], shape[i]/exp(linear_term[i]));
}
