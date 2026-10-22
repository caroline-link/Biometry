install.packages("remotes")

options(timeout = 600)
remotes::install_github("bmaitner/biometry_course",
                        subdir = "biometrydata",
                        upgrade = "never")

library(biometrydata)
?bobwhite_occupancy

# Set up
library(bbmle)
library(biometrydata)

occ <- bobwhite_occupancy
counts <- bobwhite_counts

# 10/20 occupancy model
occ_nll2 <- function(params, n_occupied, n_routes, elapsed){
  p <- plogis(params[1] + params [2] * elapsed)
  -sum(dbinom(n_occupied, size = n_routes, prob = p, log = TRUE))
}

parnames(occ_nll2) <- c("a", "b")

occ_data <- list(n_occupied = occ$n_occupied, n_routes = occ$n_routes,
                 elapsed = occ$elapsed)

m_occ <- mle2(minuslogl = occ_nll2, start = c(a = 2, b = -0.05), data = occ_data)
m_occ@min

# try adding a nonsense predictor
junk <- runif(nrow(occ))

occ_junk <-  function(params, n_occupied, n_routes, elapsed, junk){
  p <- plogis(params[1] + params [2] * elapsed + params[3] * junk)
  -sum(dbinom(n_occupied, size = n_routes, prob = p, log = TRUE))
}

parnames(occ_junk) <- c("a", "b", "c")

m_junk <- mle2(minuslogl = occ_junk,
               start = c(a = 2, b = -0.05, c = 0),
               data = c(occ_data, list(junk = junk)))
 
m_occ@min - m_junk@min # how much did nonsense "improve" the fit

# likelihood ratio test by hand and with anova
const_nll <- function(params, n_occupied, n_routes){
  p <- plogis(params[1])
  -sum(dbinom(n_occupied, size = n_routes, prob = p, log = TRUE))
}

parnames(const_nll) <- "a"

m_const <- mle2(minuslogl = const_nll, start = c(a = 1),
                data = list (n_occupied = occ$n_occupied, n_routes = occ$n_routes))

deviance_occ <- 2 * (m_const@min - m_occ@min)
deviance_occ
pchisq(deviance_occ, df = 1, lower.tail = FALSE) # the p value

anova(m_const, m_occ) # small p-value means the bigger model gained more than chance would give it 

# anova to test nonsense 
anova(m_occ, m_junk)

# three models for the counts

m_flat <- mle2(count ~ dnbinom(mu = mu0, size = k),
               start = list(mu0 = 10, k = 1.4), data = counts)

m_year <- mle2(count ~ dnbinom(mu = mu0 * exp(r * elapsed), size = k),
               start = list(mu0 = 15, r = -0.03,  k = 1.4), data = counts)

m_urban <- mle2(count ~ dnbinom(mu = mu0 * exp(b * frac_built), size = k),
                start = list(mu0 = 12, b = -3, k = 1.4), data = counts)

AIC(m_flat)
2 * m_flat@min + 2 * 2   # same number by hand (m_flat@min is the NLL, and m_flat has 2 parameters mu0 and K)

# does a model with both parameters do better than either alone?
m_both <- mle2(count ~ dnbinom(mu = mu0 * exp(b * frac_built + r * elapsed),
                               size = k),
               start = list(mu0 = 15, b = -3, r = -0.03, k = 1.4),
               data = counts)

AICtab(m_flat, m_year, m_urban, m_both, base =TRUE) # m_both is best model bc it has the lowest AIC
                                                    # dAIC (delta AIC) is how far AIC is from best model

# comparing bird species

birds <- bbs_two_species

head(birds)
table(birds$species)

# make cardinal the first level, so its a baseline in the models
birds$species <- factor(birds$species, levels = c("cardinal", "bobwhite"))

# letting a parameter differ
form <- count ~ dnbinom(mu = exp(log_mu0 + b * frac_built), size = exp(log_k))
start <- list(log_mu0 = 2, b = 0, log_k = 0)

m_same <- mle2(form, start = start, data = birds)  # one model for both species

m_mu <- mle2(form, start = start, data = birds,
             parameters = list(log_mu0 ~ species)) # a seperate mu0 for each species

coef(m_mu) # log_mu0.(Intercept) is the cardinal value

# add more parameters
m_b <- mle2(form, start = start, data = birds,
            parameters = list(b ~ species))

m_mub <- mle2(form, start = start, data = birds,
              parameters = list(b ~ species, log_mu0 ~ species))

m_muk <- mle2(form, start = start, data = birds,
              parameters = list(log_mu0 ~ species, log_k ~ species))

m_all <- mle2(form, start = start, data = birds,
              parameters = list(b ~ species, log_mu0 ~ species, log_k ~ species))

AICtab(m_b, m_mub, m_muk, m_all, base = TRUE)
