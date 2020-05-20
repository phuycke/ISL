# ------------------------------------------- #
# Introduction to Statistical Learning        #
# Chapter 03: Linear Regression (p. 71 - 139) #
# Applied exercises: exercise 10              #
# ------------------------------------------- #

rm(list = ls())

library(ISLR)

# original fit
lm.fit = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)

# leaving urban out
lm.fit2 = lm(Sales ~ Price + US, data = Carseats)
summary(lm.fit2)

anova(lm.fit, lm.fit2)

# confidence intervals
confint(lm.fit2)

# check outliers
par(mfrow = c(2, 2))
plot(lm.fit2)
