# ------------------------------------------- #
# Introduction to Statistical Learning        #
# Chapter 03: Linear Regression (p. 71 - 139) #
# Applied exercises: exercise 15              #
# ------------------------------------------- #

rm(list = ls())
dev.off()

library(MASS)

# fit each parameter seperately
par(mfrow = c(5, 3))
coefs.univar       = rep(0, 13)
coefs.univar_names = rep(0, 13)

for(indx in 2:length(Boston)){
  # create the formula for each separate predictor
  f = as.formula(paste("crim", paste(names(Boston)[indx], collapse = " + "), sep = " ~ "))
  lm.fit = lm(formula = f, data = Boston)
  
  # save the associated coefficients
  coefs.univar[indx - 1] = coef(lm.fit)[2]
  coefs.univar_names[indx - 1] = names(Boston)[indx]
    
  # plot if the p-value is smaller than 0.05
  if (summary(lm.fit)$coefficients[1,4] < .05){
    plot(x = Boston[,indx], y = Boston$crim, xlab = names(Boston)[indx])
    abline(lm.fit, lwd = 3, col = "green")
  }
}

# glue the names and the values of the coefs together
names(coefs.univar) = make.names(coefs.univar_names)

# fit a multiple regression model
lm.fit = lm(crim ~., data = Boston)
summary(lm.fit)

# get the coefs of the multiple regression
coefs.multivar = summary(lm.fit)$coefficients[2:14,1]

# plot the estimates against each other
dev.off()
plot(x = coefs.univar, y = coefs.multivar)

