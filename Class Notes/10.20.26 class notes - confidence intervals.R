library(bbmle)

occ <- read.csv("https://raw.githubusercontent.com/bmaitner/biometry_course/refs/heads/main/data/class_examples/bobwhite_occupancy.csv")
counts <- read.csv("https://raw.githubusercontent.com/bmaitner/biometry_course/refs/heads/main/data/class_examples/bobwhite_counts.csv")

nll_2019 <- function(p){
  -dbinom(52, size = 71, prob = p, log = TRUE)
}

p_vec <- seq(0.55, 0.9, by = 0.001) # candidate values of p
nll_vec <- nll_2019(p_vec)          # dbinom takes all of them at once

plot(p_vec, nll_vec, type = "l", xlab = "p", ylab = "negative log-likelihood")

qchisq(0.95, df = 1)    # deviance the true value exceeds only 5% of the time
qchisq(0.95, df= 1)/2   # the same threshold, in NLL units


# find the interval
best <- min(nll_vec)                # bottom of the curve

in_range <- nll_vec <= best + 1.92  # TRUE for every p within 1.92 of it

range(p_vec[in_range])              # the smallest and largest p that passed

# the shortcut
p_hat <- 52/71
se <- sqrt(p_hat * (1 - p_hat) / 71) # standard error of a proportion
p_hat + c(-1.96, 1.96) * se


# how sure are we about a parameter?

occ_nll2 <- function(params, n_occupied, n_routes, elapsed){
  p <- plogis(params[1] + params[2] * elapsed)
  -sum(dbinom(n_occupied, size = n_routes, prob = p, log = TRUE))
}
parnames(occ_nll2) <- c("a", "b")

occ_data <- list(n_occupied = occ$n_occupied,
                 n_routes = occ$n_routes,
                 elapsed = occ$elapsed)

m_occ <- mle2(minuslogl = occ_nll2, start = c(a = 2, b = -0.05),
              data = occ_data)
coef(m_occ)
m_occ@min      # the lowest NLL it found

# part 1: the profile
b_vec <- seq(-0.10, -0.03, by = 0.005)
prof_nll <- numeric(length(b_vec))

for (i in seq_along(b_vec)) {
  fit_i <- mle2(minuslogl = occ_nll2,
                start = c(a = 2, b = -0.05),
                fixed = list(b = b_vec[i]),
                data = occ_data)
  prof_nll[i] <- fit_i@min
}

plot(b_vec, prof_nll - m_occ@min, type = "b", ylim = c(0, 10),
     xlab = "b", ylab = "NLL above the minimum")
abline(h = 1.92, lty = 2)


# part 2: the slice
# hold a at it's best values instead of re-fitting it

a_hat <- coef(m_occ)["a"]

slice_nll <- numeric(length(b_vec))

for (i in seq_along(b_vec)){
  fit_i <- mle2(minuslogl = occ_nll2,
            start = c(a = 2, b = -0.05),
            fixed = c(a = a_hat, b = b_vec[i]),
            data = occ_data)
  slice_nll[i] <- fit_i@min
}

points(b_vec, slice_nll - m_occ@min, type = "b", col = "red")

# one line version
confint(m_occ)                  # profile intervals for every parameter
confint(m_occ, method = "quad") # the 1.96 se shourtcut

# back transform endpoints
exp(confint(m_occ)["b",])

# ````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
# Development data
urban_nll2 <- function(params, y, u ){
  mu <- params[1] * exp(params[2] * u)
  -sum(dnbinom(y, mu = mu, size = params[3], log = TRUE))
}

parnames(urban_nll2) <- c("mu0", "b", "size")

m_urban <- mle2(minuslogl = urban_nll2,
                start = c(mu0 =12, b = -3, size = 1.4),
                data = list(y = counts$count, u = counts$frac_built),
                            method = "L-BFGS-B",
                            lower = c(mu0 = 0.1, b = -20, size = 0.05),
                            upper = c(mu0 = 100, b = 20, size = 50))
                
confint(m_urban)
m_urban # gives estimate info 