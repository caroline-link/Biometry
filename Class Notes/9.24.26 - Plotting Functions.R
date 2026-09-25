# Set up
library(emdbook)
data(ReedfrogSizepred)
data(ReedfrogFuncresp)
tadpoles <- ReedfrogSizepred
kills_by_density <- ReedfrogFuncresp

ricker <- function(x, a = 1, b = 1){
  a * x * exp(-b * x)
}

michaelis_menten <- function(x, a =1, b = 1){
  a * x / (b + x)
}


# Optimization
a_try <- 124 # the asymptote
b_try <- 310 # the rate

plot(kills_by_density$Initial, kills_by_density$Killed,
     xlim = c(0,100), ylim= c(0,40))

curve(michaelis_menten(x, a = a_try, b = b_try), add = TRUE, col = "red", lwd =2)

sum((kills_by_density$Killed - 
      michaelis_menten(kills_by_density$Initial, a_try, b_try))^2)


# Power Ricker Function
power_ricker <- function(x, height, peak, alpha){
  height * (x / peak * exp(1-x/ peak))^alpha
}

alpha_try <- 35

plot(tadpoles$TBL, tadpoles$Kill, xlim = c(0,40), ylim= c(0,6))
curve(power_ricker(x, height = 4, peak = 12, alpha= alpha_try),
      add = TRUE, col = "red", lwd =2)

sum((tadpoles$Kill - power_ricker(tadpoles$TBL, 4, 12, alpha_try))^2)

# Hockey Stick
hockey_stick <- function(x,a,s){
  ifelse(x<s, a*x, a*s)
}

# Threshold
threshold <- function(x, a1, a2, s){
  ifelse(x<5, 1, 3)
}

curve(threshold(x, a1 = 1, a2 = 3, s = 5), from = 0, to = 10)

# Calculate derivatives
d_ricker <- D(expression(a * x * exp(-b * x)), "x")
d_ricker

eval(d_ricker, list(a = 1, b = 0.25, x = 1 / 0.25)) # Slope 0 at x=1/b, so that's the peak

eval(d_ricker, list( a = 2, b = 0.25, x = 0)) # Slope a at x=0, so that's the initial slope

# 3.6 R Supplement
curve(2 * x/(1 + x))

micmen <- function(x, a = 2, b = 1){
  a * x/(b + x)
}

curve(micmen(x), from = 0, to = 8, ylim = c(0,10))
curve(micmen(x, b = 3), add = TRUE, col = 2)
curve(micmen(x, a = 8), add = TRUE, col = 3)
abline(h=8)

xvec <- seq(0, 10, by = 0.1)
yvec=micmen(xvec)

plot(xvec, yvec)

## Piecewise functions
curve(ifelse(x < 5, 1, 2), from = 0, to = 10)

curve(ifelse(x < 5, 1 + 6, 6 - 3 * (x - 5)), from = 0, to = 10)

curve(ifelse(x < 5, 1 + x, ifelse( x < 8, 6-3 *
                                     (x - 5), -3 + 2 * (x - 8))),
      from = 0, to = 10)

## Derivatives
D(expression(log(x)), "x")

logist <- expression