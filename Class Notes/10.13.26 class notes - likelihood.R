bbs <- read.csv("https://raw.githubusercontent.com/bmaitner/biometry_course/refs/heads/main/data/BBS/FL_BBS.csv")

# every route surveyed in 2018, and the ones with bobwhite on them 
routes_2019 <- length(unique(bbs$Route[bbs$Year == 2019]))

with_bobwhite <- length(unique(bbs$Route[bbs$Year == 2019 &
                                         bbs$Scientific_Name == "Colinus virginianus"]))

c(routes = routes_2019, with_bobwhite = with_bobwhite)

# For later
bobwhite <- bbs$total_sightings[ bbs$Scientific_Name == "Colinus virginianus"]
length(bobwhite)

# Question w/ one parameter - if p=0.05 how likely is 52 out of 71?
dbinom(x = 52, size = 71, prob = 0.5)
dbinom(x = 52, size = 71, prob = 0.6)
dbinom(x = 52, size = 71, prob = 0.73) # highest likelihood, peak of the curve
dbinom(x = 52, size = 71, prob = 0.8)

# What if you have more than one observation?
dpois(bobwhite[1:5], lambda = 10.8) # probability of each of the first 5
prod(dpois(bobwhite[1:5], lambda = 10.8)) # all five together
prod(dpois(bobwhite[1:50], lambda = 10.8)) # fifty
prod(dpois(bobwhite, lambda = 10.8)) # all 1360

# Negative log-likelihood
binom_nll <- function(p){
  -dbinom(x = with_bobwhite, size = routes_2019, prob = p, log = TRUE)
}

binom_nll(0.5)
binom_nll(.732)

# let R find the minimum
fit <- optimize(binom_nll, interval = c(0,1))
fit$minimum # the estimate
fit$objective # the negative log-likelihood there

# Two parameters, back to back counts
mu_moment <- mean(bobwhite)
size_moment <- mu_moment^2 / (var(bobwhite) - mu_moment)

round(c(mu = mu_moment, size = size_moment), 3) # this is a shortcut because it only looks at two summaries
                                                # maximum likelihood uses whole distribution

# Fitting that
nb_nll <- function(mu, size){
  -sum(dnbinom(bobwhite, mu = mu, size = size, log = TRUE))
}

fit_nb <- optim(par = c(mu = 10, size = 1),
                fn = function(p){
                  nb_nll(p["mu"], p["size"])
                },
                method = "BFGS")

round(fit_nb$par, 3)

# the same fit, with mle2
library(bbmle)

m1 <- mle2(minuslogl  = nb_nll,
           start = list(mu = 10, size = 1),
           data = list(bobwhite = bobwhite))

coef(m1)
coef(m1) - fit_nb$par # should be about zero

# was the negative binomial worth it?
nll_poisson <- -sum(dpois(bobwhite), lamba = mean(bobwhite), log = TRUE)

# Your turn: estimate the decline
bob <- bbs[bbs$Scientific_Name == "Colinus virginianus",]
counts <- bob$total_sightings
year <- bob$Year - min(bob$Year)

trend_nll <- function(mu0, decline, size){
              mu <- mu0 * (1 - decline)^year
                -sum(dnbinom(bobwhite, mu = mu, size = size, log = TRUE))
}

fit_trend <- optim(par = c(mu0 = 15, decline = 0.03, size = 1),
                   fn = function(p){
                  trend_nll(p["mu0"], p["decline"], p["size"])            
                   },
                     method = "L-BFGS-B",
                     lower = c(0.1, -0.5, 0.5),
                     upper = c(100, 0.5, 50))

fit_trend$par

# Run again w/ a different species
counts <- bbs$total_sightings[bbs$Scientific_Name == "Progne subis"]

mean(counts)
var(counts)/ mean(counts)

nb_nll2 <- function(mu, size){
  -sum(dnbinom(counts, mu = mu, size = size, log = TRUE))
}

optim(par = c(mu = mean(counts), size = 1),
      fn = function(p){
        nb_nll2(p["mu"],p["size"])
      },
      method = "BFGS")$par

mean(counts)^2 / (var(counts) - mean(counts))