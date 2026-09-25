# Convert Celsius to Fahrenheit

c_to_f <- function(temp_c){
          (temp_c * 9/5) + 32
}

c_to_f(c(0,37,100))


# Tadpole Plotting
library(emdbook)
data(ReedfrogSizepred)
tadpoles <- ReedfrogSizepred
plot(tadpoles$TBL, tadpoles$Kill)

michaelis_menten <- function(x, a = 1, b = 1){  # a = 1 sets a default
  a * x / (b + x)
}

michaelis_menten(x=2)
michaelis_menten(x=2, a=3)

curve(michaelis_menten(x, a = 4, b = 5), from = 0, to = 40) # curve() is useful for getting used to functions

# Plot against data
plot(tadpoles$TBL, tadpoles$Kill, xlim = c(0,40), ylim = c(0,6),
     xlab = "Tadpole body length (mm)", ylab = "Number killed")
curve(michaelis_menten(x, a = 4, b = 5), add = TRUE, col = 'red', lwd = 2)

# Plot again to plot w negatove exponential f(x) = ae^-bx
negexp <- function(x, a = 1, b = 1){
  a * (exp(-b * x))
}

curve(negexp(x), from = 0, to = 7)

# Compare the two shapes, put the Michaelis-Menten curve on top
curve(michaelis_menten(x, a = 2, b = 1), add = TRUE, col = "red")

# Check w three values of a, same b
curve(negexp(x, a = 1, b = 1), from = 0, to = 7, ylim = c(0,3)) # a sets where curve starts
curve(negexp(x, a = 5, b = 1), add = TRUE, col = "red")
curve(negexp(x, a = 0, b = 1), add = TRUE, col = "blue")

# Check w same a, three values of b
curve(negexp(x, a = 1, b = 7), from = 0, to = 7, ylim = c(0,3)) # b sets how fast curve falls
curve(negexp(x , a = 1, b = 2), add = TRUE, col = "darkred")
curve(negexp(x , a = 1, b = 5), add = TRUE, col = "darkblue")

# Where does curve fall by half?
log(2) / 0.5
plot(tadpoles$TBL, tadpoles$Kill, xlim = c(0,40), ylim = c(0,6),
     xlab = "Tadpole body length (mm)", ylab = "Number killed")
curve(negexp(x, a = 6, b = 0.1), add = TRUE, col = 'red', lwd = 2)

# Ricker function
ricker <- function (x, a = 1, b = 1){
  a * x * exp(-b * x)
}

ricker(x=2)

# What happens at the ends?
ricker (0, a = 1, b = 1)
ricker(c(10, 100, 1000), a = 1, b = 1)

# Where is the peak?
xvec <- seq(0, 20, length.out = 10000)
yvec <- ricker(xvec, a = 1, b = 0.25)
xvec[which.max(yvec)]

a_try <- 1
b_try <- 0.1

curve(ricker(x, a = a_try, b = b_try), from = 0, to = 40)

# Finding peak numerically
xvec <- seq(0,40, length.out = 10000)
yvec <- ricker(xvec, a = a_try, b = b_try)

xvec[which.max(yvec)]

1/b_try

# Set a_try as 5
a_try <- 5
b_try <- 0.1

curve(ricker(x, a = a_try, b = b_try), from = 0, to = 40)

# Check goodness of fit ww/ summed squares, lower the summed squares the better
a_try <- .5
b_try <- 0.1

plot(tadpoles$TBL , tadpoles$Kill, xlim = c(0,40), ylim = c(0,6))

curve(ricker(x, a = a_try, b = b_try), add =TRUE , col = "red", lwd = 2)
sum((tadpoles$Kill - ricker(tadpoles$TBL, a_try, b_try))^2)