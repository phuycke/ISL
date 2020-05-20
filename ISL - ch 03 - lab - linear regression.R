# ------------------------------------------- #
# Introduction to Statistical Learning        #
# Chapter 03: Linear Regression (p. 71 - 127) #
# Lab: Linear Regression                      #
# ------------------------------------------- #

rm(list = ls())

library(MASS)
library(ISLR)
library(car)

#fix(Boston)
names(Boston)

# plot to have an idea first
plot(x = Boston$lstat, y = Boston$medv)
lm.fit =lm(medv ~ lstat, data = Boston)

# get an idea of the fit
lm.fit
summary(lm.fit)

coef(lm.fit)

# get the confidence interval and prediction interval
confint(lm.fit)
predict (lm.fit, 
         data.frame(lstat=c(5 ,10 ,15)),
         interval = "confidence")         # or "prediction"

# plot the regression line (intercept +- equal to 35)
abline(lm.fit, lwd = 3, col = "red")

# four diagnostic plots for the fitted object
par(mfrow = c(2,2))
plot(lm.fit)

# plot the (studentized) residuals by hand (first reset par argument)
dev.off()
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# compute the largest leverage statistic
which.max (hatvalues(lm.fit))

# Multiple linear regression for specific predictors
lm.fit = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)
dev.off()

# Multiple linear regression for all predictors
lm.fit = lm(medv ~ ., data = Boston)
summary(lm.fit)

par(mfrow = c(2, 2))
plot(lm.fit)
dev.off()

summary(lm.fit)$r.sq     # get the r squared
summary(lm.fit)$sigma    # get the RSE
vif(lm.fit)              # get variance inflation factor

# fit without a specific variable (method 1)
lm.fit1 = lm(medv ~ .-age, data = Boston)
summary(lm.fit1)

# fit without a specific variable (method 2)
lm.fit1 = update(lm.fit, ~.-age)
summary(lm.fit1)

# interaction terms
   # lstat:black -> the interaction between black and lstat
   # lstat*black -> the main effects of lstat and black, and the interaction between lstat and black

summary(lm(medv ~ lstat*age, data = Boston)) 
summary(lm(medv ~ lstat + age + lstat:age, data = Boston))    # equal to the former formula

# non linear transformation of predictors
lm.fit2 = lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)

# check whether a model is better than the other or not
anova(lm.fit, lm.fit2)

par(mfrow = c(2, 2))
plot(lm.fit2)        # the first plot indicates that the non linearity has been dealt with

# create a polynomial fit of the fifth order
lm.fit5 = lm(medv ~ poly(lstat ,5), data = Boston)
summary(lm.fit5)

# log transformation
summary(lm(medv ~ log(rm), data = Boston))

# Qualitative predictors 
# Now we are working with the carseats dataset
names(Carseats)

# try a linear fit with some categorical variables
lm.fit = lm(Sales ~.+Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

contrasts(Carseats$ShelveLoc)      # get the contrasts used

# a small intro to functions
# write the function
loadLibs = function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

# load the function
loadLibs()













