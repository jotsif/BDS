library("rstan")

###
### Generate data set
###

setsize <- 1000

### Lognormal distribution gives median income of ~ round 250k, and mean income of ~ 400k which is similar to distribution in Sweden
income <- rlnorm(setsize, meanlog = log(250000), sdlog = 1)

### Gamma distributed age depending on income gives reasonably realistic age distribution
age <- rgamma(setsize, shape = 15 * income**(1/5)/17, scale = 4 * income**(1/5)/15)

beta0 <- 0.4
beta1 <- -2/100
beta2 <- -0.05
beta3 <- -0.1/10**4
beta4 <- 15/10**6
a <- 5

default.prob <- sapply(a * (beta0 + beta1 * age + beta4 * age ** 2 + beta2 * log(income) + beta3 * log(income)**2), function(x) {exp(x)/(1 + exp(x))})

credit.dataset <- subset(data.frame(age = age, income = income, default = as.numeric(runif(setsize, 0, 1) < default.prob)), age >= 18 & age < 100)

### Dataset visualisation
ggplot(data = credit.dataset, aes(x = age, y = income, col = factor(default))) + geom_point() + scale_y_log10() + xlab("Age") + ylab("Income")
ggsave("scatter_age_income.pdf")

### Dataset visualisation
ggplot(data = credit.dataset, aes(x = age)) + geom_histogram() + xlab("Age")
ggsave("age_histogram.pdf")

ggplot(data = credit.dataset, aes(x = income)) + geom_histogram() + scale_x_log10() + xlab("Income")
ggsave("income_histogram.pdf")


### Stan modelling
trainset <- sample(x = c(0, 1), size = nrow(credit.dataset), replace = TRUE, prob = c(0.5, 0.5)) == 1

model.data <- list(
    N = sum(trainset),
    p = 3,
    defaultd = credit.dataset$default[trainset],
    age = credit.dataset$age[trainset],
    income = credit.dataset$income[trainset]
)

logreg.model <- stan_model(file = 'src/stan/bayesian_classification.stan')

fit <- sampling(logreg.model, data = model.data, cores = 8, iter = 1000, init = 0)

pairs(fit, pars = c("beta[1]", "beta[2]", "beta[3]"))

### TODO> BUILD STANDARD LOG REGRE

sample.result <- extract(fit)

### Plot prob distribution
hist(sample.result$prob[, 1], 100)

### Plot mean against sd
my.mean <- apply(sample.result$prob, 2, mean)
my.sd <- apply(sample.result$prob, 2, sd)

ggplot(data = data.frame(mean = my.mean, sd = my.sd, income = credit.dataset$income[trainset], age = credit.dataset$age[trainset]), aes(x = mean, y = sd, col = income < 150000 & age > 20)) + geom_point() + scale_x_log10() + scale_y_log10() + xlab("Mean probability") + ylab("Sample deviation")
ggsave("uncertainty_vs_prob_and_variables.pdf")


ggplot(data = rbind(data.frame(sample = "Low probability", dist = sample.result$prob[, 1]), data.frame(sample = "High probability", dist = sample.result$prob[, 78])), aes(x = dist, fill = sample)) + geom_histogram(position = 'dodge') + xlab("Estimated probability")
ggsave("two_sample_comparison.pdf")
