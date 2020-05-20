# ------------------------------------------- #
# Introduction to Statistical Learning        #
# Chapter 03: Linear Regression (p. 71 - 139) #
# Applied exercises: exercise 12              #
# ------------------------------------------- #

rm(list = ls())

set.seed (1)
x = rnorm (100)
y = 2 * x + rnorm(100)

# estimation without an intercept: different estimation values
lm.fit1 = lm(y ~ x+0)
summary(lm.fit1)

lm.fit2 = lm(x ~ y+0)
summary(lm.fit2)

# estimation without an intercept: similar estimations
standardize = function(vec){
  return((vec - mean(vec))/sd(vec))
}

x = rnorm(100)
y = rnorm(100)

x = standardize(x)
y = standardize(y)

# estimation without an intercept: different estimation values
lm.fit1 = lm(y ~ x+0)
summary(lm.fit1)

lm.fit2 = lm(x ~ y+0)
summary(lm.fit2)

