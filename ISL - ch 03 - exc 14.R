# ------------------------------------------- #
# Introduction to Statistical Learning        #
# Chapter 03: Linear Regression (p. 71 - 139) #
# Applied exercises: exercise 14              #
# ------------------------------------------- #

rm(list = ls())

set.seed(1)

x1 = runif(100)
x2 = 0.5* x1 + rnorm(100)/10
y  = 2+2* x1 + 0.3*x2 + rnorm(100)

plot(x = x1, y = x2)

# OLS 
lm.fit = lm(y ~ x1 + x2)     # x1 significant, x2 not
summary(lm.fit)
par(mfrow = c(2, 2))
plot(lm.fit)

lm.fit = lm(y ~ x1)          # x1 significant
summary(lm.fit)

lm.fit = lm(y ~ x2)          # x2 significant
summary(lm.fit)

# add a new observation and do the analyses all over again
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y  = c(y, 6)

plot(x1, x2)

lm.fit = lm(y ~ x1 + x2)     # x1 significant, x2 not
summary(lm.fit)
par(mfrow = c(2, 2))
plot(lm.fit)

lm.fit = lm(y ~ x1)          # x1 significant
summary(lm.fit)

lm.fit = lm(y ~ x2)          # x2 significant
summary(lm.fit)
