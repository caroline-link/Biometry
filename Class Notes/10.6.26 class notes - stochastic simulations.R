a <-2
b <- 1
x <- 1:20

y_det <- a + b * x

plot(x = x, y = y_det)

# add noise
y_stoch <- rnorm(n = 20, mean = y_det, sd = 2)

plot(x = x, y = y_stoch)

# when the noise grows with the mean
sd_multiplier <- 0.3

y_stoch <- rnorm(n = 20, mean = y_det, sd = y_det * sd_multiplier)
plot(x = x, y = y_stoch)
points(x, y_det, col = "blue")

# how big does sd have to be before the line is invisible in the data?
a <- 2
b <- -1
x <- 1:20
y_det <- a + b * x
y_stoch <- rnorm(n = 20, mean = y_det, sd = 30)
plot(x = x, y = y_stoch)
abline(a = a, b = b, col = "red")

# adding negative binomial noise
a <- 20
b <- 1
k <- 5
x <- runif(50, min = 0, max = 5)

y_det <- a * b / (b + x)
y <- rnbinom(n = 50, mu = y_det, size = k)

plot(x = x, y = y)

# changing parameters, what do they do?
a <- 20
b <- 30
k <- 5
x <- runif(50, min = 0, max = 5)

y_det <- a * b / (b + x)
y <- rnbinom(n = 50, mu = y_det, size = k)

plot(x = x, y = y)
points(x, y_det, col = "blue", pch = 16)

# k changes how closely the points are distributed around the curve (the spread)
# a is the y intercept for the curve
# b changes slope of curve

# Distributions where subgroups have different parameters
## ex. 2 species w 25 observations each
g <- factor(rep(1:2, each = 25))

a <- c(20, 10)
b <- c(1,2)

# Indexing by group
x <- runif(50, min = 0, max = 5)

y_det <- a[g] * b[g] / (b[g] + x) # a[g] reads "the a for whichever group this observation belongs to 

plot(x = x, y = y_det, col = g, pch = 16)

# Add noise
k <- 5

y <- rnbinom(n = 50, mu = y_det, size = k)

plot(x = x, y = y, col = g, pch = as.numeric(g))
legend("topright", bty = "n", col = 1:2, pch = 1:2,
       legend = c("species 1","species 2"))

# find parameters where two species look clearly different
a <- c(20, 100)
b <- c(1,10)
k <- 50

y_det <- a[g] * b[g] / (b[g] + x)
y <- rnbinom(n = 50, mu = y_det, size = k)
plot(x = x, y = y, col = g, pch = as.numeric(g))

# find parameters where two species are identical 
a <- c(20, 15)
b <- c(1,1)
k <- 10

y_det <- a[g] * b[g] / (b[g] + x)
y <- rnbinom(n = 50, mu = y_det, size = k)
plot(x = x, y = y, col = g, pch = as.numeric(g))

# Example 1: frog experiment
library(emdbook)
frogs <- emdbook::ReedfrogFuncresp

p <- (60*frogs$Initial / (100 + frogs$Initial)) / frogs$Initial

killed_sim <- rbinom(n = nrow(frogs), size = frogs$Initial, prob = p)

plot(frogs$Initial, killed_sim, pch = 16, col = "red",
     xlab = "tadpoles in the tank", ylab = "number killed") # red points are experimental
points(frogs$Initial, frogs$Killed, pch = 1, cex = 1.4)

# Example 2: counting cardinals
