# ------------------------------------------- #
# Introduction to Statistical Learning        #
# Chapter 03: Linear Regression (p. 71 - 139) #
# Applied exercises: exercise 09              #
# ------------------------------------------- #

rm(list = ls())

library(MASS)

# scatterplot for all variables
plot(Auto)

# correlations for only the numeric variables
cor(Auto[sapply(Auto, is.numeric)])

# Multiple linear regression
lm.fit = lm(mpg ~.-name, data = Auto)
summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)
dev.off()

# Multiple linear regression with some interactions
lm.fit = lm(mpg ~ .-name + weight:year + weight:acceleration, data = Auto)
summary(lm.fit)

# transform the predictors
par(mfrow = c(2, 2))

## original fit
plot(x = Auto$horsepower, y = Auto$mpg)
lm.fit = lm(mpg ~ horsepower, data = Auto)
abline(lm.fit, lwd = 3, col = "red")
summary(lm.fit)

## logscaling the predictor
lm.fit2 = lm(mpg ~ log(horsepower), data = Auto)
plot(x = log(Auto$horsepower), y = Auto$mpg)
abline(lm.fit2, lwd = 3, col = "blue")
summary(lm.fit2)

# taking the square root of the predictor
lm.fit3 = lm(mpg ~ sqrt(horsepower), data = Auto)
plot(x = sqrt(Auto$horsepower), y = Auto$mpg)
abline(lm.fit3, lwd = 3, col = "green")
summary(lm.fit3)

# squaring the predictor
lm.fit4 = lm(mpg ~ I(horsepower^2), data = Auto)
plot(x = (Auto$horsepower)^2, y = Auto$mpg)
abline(lm.fit4, lwd = 3, col = "black")
summary(lm.fit4)

# comparing the resulting models
anova(lm.fit, lm.fit2)
anova(lm.fit, lm.fit3)
anova(lm.fit, lm.fit4)

dev.off()
