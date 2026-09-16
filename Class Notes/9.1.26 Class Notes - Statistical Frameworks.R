2*8

sqrt(25)

x<-sqrt(36)

x
# ? gives general R documentation 
?sqrt

# example(package) walks you through an example of the function
example(sqrt)

# help.search("") gives a list of packages/functions that you could use 
help.search("correlation")

## Build a dataset where we know the true answer, mean=2*frogs and sd=0.5, and see whether our analysis can cover it.

frogs<- c(1.1, 1.3, 1.7, 1.8, 1.9, 2.1, 
          2.3, 2.4, 2.5, 2.8, 3.1, 3.3, 
          3.6, 3.7, 3.9, 4.1, 4.5, 4.8,
          5.1, 5.3)

# add the noise around signal
set.seed(101)
tadpoles <- rnorm(n=20,
                  mean = 2*frogs,
                  sd = 0.5)

# plot
plot(x=frogs, y=tadpoles)
abline (a=0,b=2)

# make it more noisy
tadpoles_noisy<- rnorm(20,2*frogs, sd=3)
plot(frogs, tadpoles_noisy)
abline(a=0, b=2)

# can we recover the truth?
coef(lm(tadpoles~frogs))
coef(lm(tadpoles_noisy~frogs))
confint(lm(tadpoles~frogs))

# Replicate simulation 1000 times
slopes_clean <- replicate (1000,
                           coef(lm(rnorm(20, 2*frogs, sd=0.5)~frogs))[2])

slopes_noisy <-replicate (1000,
                          coef(lm(rnorm(20, 2*frogs, sd=3)~frogs))[2])

# Let's summarize what we found:
mean(slopes_clean)
mean(slopes_noisy)
sd(slopes_clean)
sd(slopes_noisy)

# Summarize and test
mean(tadpoles)
summary(tadpoles)
cor(frogs,tadpoles)
cor.test(frogs, tadpoles)

# Run again
set.seed(202)
tadpoles2 <- rnorm(20, 2*frogs, sd=0.5)
cor(frogs, tadpoles2)