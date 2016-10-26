data {
  int<lower=0> N;
  real income[N];
}
parameters {
  real<lower=0> mmean;
  real<lower=0> msd;
}
model {
  income ~ normal(mmean, msd);
}
