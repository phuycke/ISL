# ------------------------------------------- #
# Introduction to Statistical Learning        #
# Chapter 03: Linear Regression (p. 71 - 139) #
# Applied exercises: exercise 08              #
# ------------------------------------------- #

rm(list = ls())

library(MASS)

lm.fit = lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)

# diagnostic plots
par(mfrow = c(2, 2))
plot(lm.fit)
dev.off()

# prediction interval
predict(lm.fit, 
        data.frame(horsepower=c(98)),
        interval = "prediction")         # or "prediction"

# confidence interval
predict(lm.fit, 
        data.frame(horsepower=c(98)),
        interval = "confidence")         # or "prediction"

plot(x = Auto$horsepower, y = Auto$mpg)
abline(lm.fit, lwd = 3, col = "red")

