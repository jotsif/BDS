library("rstan")
library("ggplot2")

incomes <- read.table("data/DP_LIVE_17102016202817844.csv", sep = ',', header = TRUE, stringsAsFactors=FALSE)

disposable.income.2012 <- subset(incomes, MEASURE == "USD_CAP" & TIME == "2012" & LOCATION != "EU" & LOCATION != "EA")

ggplot(data = disposable.income.2012, aes(x = Value)) + geom_histogram(fill = rgb(215/255, 60/255, 255/255)) + xlab("Household disposable income") + ylab("Count")
ggsave("income_distribution.pdf")

income.vector <- disposable.income.2012$Value

model.data <- list(
    N = length(income.vector),
    income = income.vector
)

mean.model <- stan_model(file = 'src/stan/mean.stan')

fit <- sampling(mean.model, data = model.data, cores = 8, iter = 1000, init = 0)

posterior.data.no.outlier <- extract(fit)

pdf("simulation.pdf")
pairs(fit, pars = c("alpha", "beta"))
dev.off()

norm.sampling <- as.vector(mapply(function(mu, sd) {rgamma(n = 100, shape = mu, scale = sd)}, posterior.data.no.outlier$alpha, posterior.data.no.outlier$beta))

ggplot(data = rbind(data.frame(Simulated = "No", income = income.vector), data.frame(Simulated = "Yes", income = norm.sampling)) , aes(x = income, fill = Simulated, y = ..density..)) + geom_histogram(position = "dodge") + scale_fill_manual(values = c(rgb(215/255, 60/255, 255/255), rgb(0/255, 60/255, 255/255)))  + xlab("Household disposable income") + ylab("Count")
ggsave("income_distribution_with_simulated_data_no_outlier.pdf")

### LETS ADD "Taiwan"

income.with.outlier <- c(income.vector, 1* 10**5)

ggplot(data = data.frame(disposable.income = income.with.outlier), aes(x = disposable.income)) + geom_histogram(fill = rgb(215/255, 60/255, 255/255)) + xlab("Household disposable income") + ylab("Count")
ggsave("income_distribution_with_outlier.pdf")

model.data <- list(
    N = length(income.with.outlier),
    income = income.with.outlier
)

fit <- sampling(mean.model, data = model.data, cores = 8, iter = 1000, init = 0)

posterior.data.outlier <- extract(fit)

norm.sampling.outlier <- as.vector(mapply(function(mu, sd) {rgamma(n = 100, shape = mu, scale = sd)}, posterior.data.outlier$alpha, posterior.data.outlier$beta))

ggplot(data = rbind(data.frame(Simulated = "No", income = income.with.outlier), data.frame(Simulated = "Yes - without outlier", income = norm.sampling), data.frame(Simulated = "Yes - with outlier", income = norm.sampling.outlier)) , aes(x = income, fill = Simulated, y = ..density..)) + geom_histogram(position = "dodge") + scale_fill_manual(values = c(rgb(215/255, 60/255, 255/255), rgb(0/255, 60/255, 255/255), rgb(95/255, 198/255, 168/255)))  + xlab("Household disposable income") + ylab("Count") + xlim(c(0, 110000))
ggsave("income_distribution_with_simulation_and_outlier.pdf")
