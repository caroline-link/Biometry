# d for density
x_vector <- seq(from = -10, to= 10, by = 0.1)

plot(dnorm(x = x_vector, mean = 0, sd = 1) ~ x_vector)

# change mean, keep sd the same (mean shifts plot to left or right)
plot(dnorm(x = x_vector, mean = 10, sd = 1)~ x_vector, type = "l") 
plot(dnorm(x = x_vector, mean = 2, sd = 1)~ x_vector, type = "l")

# keep mean, change sd (sd changes shape of curve)
plot(dnorm(x = x_vector, mean = 0, sd = 10)~ x_vector, type = "l")
plot(dnorm(x = x_vector, mean = 0, sd = .1)~ x_vector, type = "l")

# pnorm() for probablility, give it a value, it gives you the probability
pnorm(q = 0, mean = 0, sd = 1)

# qnorm() for quantile, give it a probability, it gives you a value
qnorm(p = 0.5, mean = 0, sd = 1)

# rnorm() for random
set.seed(42)
rnorm(n = 5, mean = 0, sd = 1)

# simulate and compare
draws <- rnorm(n = 100, mean = 10, sd = 2)
hist(draws, freq = FALSE)
curve(dnorm(x, mean = 10, sd = 2), add = TRUE, col = "red", lwd = 2)

draws <- rnorm(n = 1000, mean = 10, sd = 2)
hist(draws, freq = FALSE)
curve(dnorm(x, mean = 10, sd = 2), add = TRUE, col = "red", lwd = 2)

# binomal 
k <- seq(from = 0, to = 20, by = 1)
plot(dbinom(x = k, size = 20, prob = 0.5) ~ k, ylim = c(0, 0.3))

plot(dbinom(x = k, size = 20, prob = 0.1) ~ k, ylim = c(0, 0.3)) # prob chnages skew of the curve
plot(dbinom(x = k, size = 20, prob = 0.9) ~ k, ylim = c(0, 0.3))
plot(dbinom(x = k, size = 5, prob = 0.5) ~ k, ylim = c(0, 0.3)) # size changes height?


