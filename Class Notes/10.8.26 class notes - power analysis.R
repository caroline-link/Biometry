bbs <- read.csv("https://raw.githubusercontent.com/bmaitner/biometry_course/refs/heads/main/data/BBS/FL_BBS.csv")

bobwhite <- bbs[bbs$Scientific_Name == "Colinus virginianus",]

nrow(bobwhite)
head(bobwhite[, c("Year", "Route", "total_sightings")])

# testing if Poisson distribution fits
counts_real <- bobwhite$total_sightings

mean(counts_real) # observed: variance is 111, p(count>40)=0.024
var(counts_real)

mean(counts_real >= 40)

ppois(39, lambda = mean(counts_real), lower.tail = FALSE) # Poisson: variance is 11, p(count>=40)=7.00e-12

# getting parameters from data
mu0 <- mean(bobwhite$total_sightings)
v <- var(bobwhite$total_sightings)

size <- mu0^2 / (v - mu0)

round(c(mean = mu0, variance = v, sizw = size),2)

mu0 + mu0^2 / size

# does negative binomial pass the same test?
pnbinom(39, mu = mu0, size = size, lower.tail = FALSE) # negbinomial: p(count>=40)=0.0233

# model is settled, need to choose study design & how to alocate 400 visits over time and space
# Step 1: the expected counts
mu0 <- 10.8
k <- 1.15
d <- 0.03

n_routes <- 40
n_years <- 18

year <- rep(0:(n_years - 1), each = n_routes)

mu <- mu0 * (1 - d) ^ year
plot(year, mu, ylim = c(0,12))

# Step 2: add noise
counts <- rnbinom(n = length(year), mu = mu, size = k)

plot(jitter(year), counts)
points(year, mu, col = "red", pch = 16)

# Step 3: analyze it like real data
m <- lm(log(counts + 1) ~ year)

summary(m)$coefficients["year",] 

# compute the power of one design (40 routes over 10 years)
year <- rep(0:9, each = 40) 
mu <- 10.8 * (1 - 0.03) ^ year

found <- numeric(500)

for( i in 1:500){
  counts <- rnbinom(n = length(year), mu = mu, size = 1.15)
  m <- lm(log(counts + 1) ~ year)
  found[i] <- summary(m)$coefficients["year", "Pr(>|t|)"] < 0.5
}

# found[i] creates a vector where a run w/ significant result = 1, insignificant result = 0
mean(found) 

# Is it easier to detect a fast or slow decline?
year <- rep(0:9, each = 40)
decline <- 0.3

found <- numeric(500)

for(i in 1:500){
  counts <- rnbinom(length(year), mu = 10.8 * (1 - decline) ^ year, size = 1.15)
  m <- lm(log(counts + 1) ~ year)
  found[i] <- summary(m)$coefficients["year", "Pr(>|t|)"] < 0.5
}

mean(found) # easier to detect fast decline
---------------------------------------------------------------------------------------------
# Do protected reefs hold more fish? How many surveys per group would it take to find out?

reef <- read.csv("https://raw.githubusercontent.com/bmaitner/biometry_course/refs/heads/main/data/Reef_fish/NCRMP_reef_fish_surveys.csv")

reef$protected <- ifelse(reef$PROT == 0, "no", "yes")

# Deterministic part is a pair of means
unprot <- reef$total_abundance[reef$protected == "no"]
prot <- reef$total_abundance[reef$protected == "yes"]

hist(unprot)
hist(log(unprot))

hist(prot)
hist(log(prot))

# Which distribution?
mean(unprot > 500) # observed = 0.055

mean <- log10(mean(unprot))
sd <- log10(sd(unprot))

plnorm(500, meanlog = mean, sdlog = sd, lower.tail = FALSE) # lognorm = 0.0445

# the parameters
meanlog_unprot <- mean(log(unprot + 1)) # added 1 bc 0 values meant that mean(log(unprot)) = -INF
sdlog_unprot <- sd(log(unprot + 1))

meanlog_prot <- mean(log(prot + 1))
sdlog_prot <- sd(log(prot + 1))

meanlog_unprot
meanlog_prot

sdlog_unprot
sdlog_prot

# for 120 surveys total, how many sites in each group and which effect size you want to be able to detect?
n_per_group <- 60 
effect <- 1.5 # the ratio you want to be able to detect

# simulate, analyze, and repeat
sim_unprot <- rlnorm(n_per_group, meanlog = meanlog_unprot, sd = sdlog_unprot)
sim_prot <- rlnorm(n_per_group, meanlog = meanlog_unprot + log(effect), sd = sdlog_prot)

m <- lm(log(counts + 1) ~ year)

summary(m)$coefficients["year",] 