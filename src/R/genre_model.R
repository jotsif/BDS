library("rstan")

genre.danceability <- read.table("data/danceability_set.csv", sep = ';', header = TRUE)

top.10.genres <- tail(sort(table(genre.danceability$genres)), 10)

first.set <- sample(c(0, 1), size = nrow(genre.danceability), replace = TRUE, prob = c(0.50, 0.50)) == 1

model.set <- subset(genre.danceability, genres %in% names(top.10.genres) & first.set)

model.set$genre.index <- as.integer(factor(model.set$genres))

model.set$artist_plays_previous_month <- ifelse(is.na(model.set$artist_plays_previous_month), 0, model.set$artist_plays_previous_month)

model.data <- list(
    N = nrow(model.set),
    n_genres= length(unique(model.set$genres)),
    plays = model.set$plays,
    previous_plays = model.set$artist_plays_previous_month + 1,
    genre_index = model.set$genre.index,
    danceability = model.set$danceability
)

plays.model <- stan_model(file = "src/stan/plays.stan")

plays.fit <- sampling(plays.model, data = model.data, cores = 8, iter = 2000, init = 1)


pairs(plays.fit, pars = c("mu_beta0", "mu_beta1", "mu_beta2", "mu_beta3", "mu_beta4"))

posterior <- extract(plays.fit)

model.set$simulated.plays <- exp(colMeans(posterior$linear_term))

### Check fit
y <- with(model.set, tapply(plays, cut(simulated.plays, breaks = quantile(simulated.plays, probs = seq(0, 1, 0.05))), mean))
x <- with(model.set, tapply(simulated.plays, cut(simulated.plays, breaks = quantile(simulated.plays, probs = seq(0, 1, 0.05))), mean))
plot(log(x), log(y))


genres <- unique(model.set[, c("genres", "genre.index")])

ggplot(data = merge(data.frame(melt(posterior$beta0[, ])), genres, by.x = "Var2", by.y = "genre.index"), aes(x = factor(genres), y = value)) + geom_violin() + xlab("Genres") + ylab("Beta0")
ggsave("beta0_genre_model.pdf")

ggplot(data = merge(data.frame(melt(posterior$beta1[, ])), genres, by.x = "Var2", by.y = "genre.index"), aes(x = factor(genres), y = value)) + geom_violin() + xlab("Genres") + ylab("Beta1")
ggsave("beta1_genre_model.pdf")

ggplot(data = merge(data.frame(melt(posterior$beta2[, ])), genres, by.x = "Var2", by.y = "genre.index"), aes(x = factor(genres), y = value)) + geom_violin() + xlab("Genres") + ylab("Beta2")
ggsave("beta2_genre_model.pdf")

ggplot(data = merge(data.frame(melt(posterior$beta3[, ])), genres, by.x = "Var2", by.y = "genre.index"), aes(x = factor(genres), y = value)) + geom_violin() + xlab("Genres") + ylab("Beta2")
ggsave("beta3_genre_model.pdf")


model.set$simulated.plays.95th <- exp(apply(posterior$linear_term, 2, function(i) {sort(i)[3900]}))
model.set$simulated.plays.5th <- exp(apply(posterior$linear_term, 2, function(i) {sort(i)[100]}))
genre.mean <- with(model.set, aggregate(simulated.plays, list(genre = factor(genres)), mean))

size = 0.01
plot.genre <- "NewAge"
denominator <- subset(genre.mean, genre == plot.genre)$x
dance <- with(subset(model.set, genres == plot.genre), tapply(danceability, cut(danceability, breaks = quantile(danceability, probs = seq(0, 1, size))), mean))
simulated <- with(subset(model.set, genres == plot.genre), tapply(simulated.plays, cut(danceability, breaks = quantile(danceability, probs = seq(0, 1, size))), mean))/denominator
simulated.max <- with(subset(model.set, genres == plot.genre), tapply(simulated.plays.95th, cut(danceability, breaks = quantile(danceability, probs = seq(0, 1, size))), mean))/denominator
simulated.min <- with(subset(model.set, genres == plot.genre), tapply(simulated.plays.5th, cut(danceability, breaks = quantile(danceability, probs = seq(0, 1, size))), mean))/denominator
actual <- with(subset(model.set, genres == plot.genre), tapply(plays, cut(danceability, breaks = quantile(danceability, probs = seq(0, 1, size))), mean))/denominator
ggplot(data = data.frame(x = dance, y1 = simulated, y2 = actual, y1.max = simulated.max, y1.min = simulated.min), aes(x = x)) + geom_ribbon(aes(ymax = y1.max, ymin = y1.min), alpha = 0.3) + geom_line(aes(y = y1), col = rgb(95/255, 198/255, 168/255)) + geom_line(aes(y = y2), col = rgb(0/255, 60/255, 255/255), alpha = 0.2) + scale_y_log10() + xlab("Danceability") + ylab("Plays")
ggsave(paste("danceability", plot.genre, "vs_plays.pdf", sep = "_"))
