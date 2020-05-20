# ------------------------------------------- #
# Introduction to Statistical Learning        #
# Chapter 03: Linear Regression (p. 71 - 139) #
# Applied exercises: exercise 13              #
# ------------------------------------------- #

rm(list = ls())

set.seed(1)

x   = rnorm(100, 0, 1)
eps = rnorm(100, 0, .075)
y   = -1 + .5*x + eps 

# scatterplot
plot(x = x, y = y)

# OLS model
lm.fit = lm(y ~ x)
summary(lm.fit)

abline(lm.fit, lwd = 1, col = 'red')
abline(a = -1, b = .5, col = 'green')
legend("topleft", legend=c("Least squares", "Population"),
       col=c("red", "green"), lty=1:2, cex=0.8)

# polynomial fit
lm.fit2 = lm(y ~ x + I(x^2))
summary(lm.fit2)

confint(lm.fit)
