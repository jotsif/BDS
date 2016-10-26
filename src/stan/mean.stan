data {
  int<lower=0> N;
  real income[N];
}
parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
}
model {
  income ~ gamma(alpha, 1/beta);
}
