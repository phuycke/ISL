# ------------------------------------------- #
# Introduction to Statistical Learning        #
# Chapter 03: Linear Regression (p. 71 - 139) #
# Applied exercises: exercise 11              #
# ------------------------------------------- #

rm(list = ls())

# generate the predictors and their associated response
set.seed (1)
x = rnorm (100)
y = 2 * x + rnorm(100)

# set the linear regression without the intercept
lm.fit = lm(y ~ x+0)
summary(lm.fit)

# do the analysis with an intercept
lm.fit2= lm(y ~ x)
summary(lm.fit2)

# by hand estimation of beta and the se of beta (see formula 3.38)

# estimation of the beta in the intercept without beta zero
manual.beta_estim = sum(x * y)  / sum(x^2)

# standard error of this parameter estimation
manual.beta_std_err = sqrt(sum((y - x*manual.beta_estim)^2) / ((length(x)-1)*sum(x^2)))

# t statistic for this model
manual.t_statistic = ((sqrt(length(x) - 1)) * sum(x * y)) / (sqrt(sum(x^2)*sum(y^2) - sum(x*y)^2))
# Note that the t-statistic is in essence the same as manual.beta_estim / manual.beta_std_err

summary(lm.fit)

