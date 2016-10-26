data {
  int<lower=0> N;
  int<lower=0> p;
  real income[N];
  real age[N];
  int defaultd[N];
}
parameters {
  // BETA0
  real <lower=-30,upper=30> beta[p];
}
transformed parameters {
  real<lower=0> odds[N];
  real<lower=0, upper=1> prob[N];

  for (i in 1:N) {
    odds[i] <- exp(beta[1] + beta[2]*log(income[i]) + beta[3]*age[i]);
    prob[i] <- odds[i] / (odds[i] + 1);
  }
}
model {
  defaultd ~ bernoulli(prob);
}
