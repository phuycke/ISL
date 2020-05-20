# -------------------------------------------- #
# Introduction to Statistical Learning         #
# Chapter 03: Linear Regression (p. 127 - 175) #
# Lab: Logistic Regression, LDA, QDA, and KNN  #
# -------------------------------------------- #

rm(list = ls())

library(ISLR)

names(Smarket)
dim(Smarket)
summary(Smarket)
?Smarket

# basically the same as plot(Smarket)
pairs(Smarket)

# plot the correlations
cor(Smarket[,-9])

# see how volume evolves over time
plot(Smarket$Volume)

# Logistic regression: fit a model
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit)

# get the coefficients
coef(glm.fit)
summary(glm.fit)$coef


# based on this, we can see that we predict the probability that the market goes up, since 1 = up
contrasts(Smarket$Direction)

# predict the direction of the stock market for the training data we used to fit the model
glm.prob = predict(glm.fit, type = "response")
glm.prob[1:10]

# convert the predictions to the labels used in the dataset
glm.pred = rep("Down", dim(Smarket)[1])
glm.pred[glm.prob > .5] = "Up"
glm.pred[1:10]

# make a confusion matrix based
table(glm.pred, Smarket$Direction)

# accuracy in %
mean(glm.pred == Smarket$Direction) * 100

# train the model on a trainingset
train      = (Smarket$Year < 2005)
data.train = Smarket[train,]
data.test  = Smarket[!train,]

# fit the model on the training data
glm.train = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = data.train, family = binomial)

# make a prediction for the test data based on the model fitted on the train data and change the labels
glm.prob  = predict(glm.train,
                    newdata = data.test,
                    type    = "response")
glm.pred = rep("Down", dim(data.test)[1])
glm.pred[glm.prob > 0.5] = "Up"

# make a confusion matrix and check the accuracy
table(glm.pred, data.test$Direction)
mean(glm.pred == data.test$Direction) * 100

# refit the model using only lag1 and lag2
glm.train = glm(Direction ~ Lag1 + Lag2, data = data.train, family = binomial)

# make a prediction for the test data based on the model fitted on the train data and change the labels
glm.prob  = predict(glm.train,
                    newdata = data.test,
                    type    = "response")
glm.pred = rep("Down", dim(data.test)[1])
glm.pred[glm.prob > 0.5] = "Up"

# make a confusion matrix and check the accuracy
table(glm.pred, data.test$Direction)
mean(glm.pred == data.test$Direction) * 100

# predict the stock market direction for specific values for Lag1 and Lag2
predict(glm.train,
        newdata = data.frame(Lag1 = c(1.2, 1.5),
                             Lag2 = c(1.1, -.8)),
        type = "response")

# LDA 

library(MASS)

lda.fit = lda(Direction ~ Lag1 + Lag2, data = data.train)
lda.fit

# plots of the linear discriminants, obtained by computing −0.642 × Lag1 − 0.514 × Lag2 for each of the training observations.
plot(lda.fit)

# predictions based on the LDA
lda.pred = predict(lda.fit, 
                   newdata = data.test,
                   type = "response")

# make a confusion matrix based on the labels outputted by the LDA
table(lda.pred$class, data.test$Direction)
mean(lda.pred$class == data.test$Direction)

# manually compute the amount of down and up predictions respectively
sum(lda.pred$posterior[ ,1] >= .5)
sum(lda.pred$posterior[ ,1]  < .5)

# Thus, note that the posterior probability output by the model corresponds to the probability that the market will decrease

# change the decision threshold: predict decrease if the posterior prob that the market decreases is >90%
sum(lda.pred$posterior[,1] > .9)
# there is no posterior prob that large, which explains why it is so difficult to predict market decreases/increases

# QDA

qda.fit = qda(Direction ~ Lag1 + Lag2, data = data.train)
qda.fit

qda.pred = predict(qda.fit, 
                   newdata = data.test,
                   type = "response")
table(qda.pred$class, data.test$Direction)
mean(lda.pred$class == data.test$Direction)

# KNN

library(class)

# get the data of the predictors in the training set
train.X = cbind(Smarket$Lag1, Smarket$Lag2)[train,]
test.X  = cbind(Smarket$Lag1, Smarket$Lag2)[!train,]

train.Direction = Smarket$Direction[train]

set.seed(1)
# flexible fit
knn.pred = knn(train.X,
               test.X,
               train.Direction,
               k = 1)

# the overly flexible fit yields bad results
table(knn.pred, data.test$Direction)
(83+43) / 2.52

# more conservative fit
knn.pred = knn(train.X,
               test.X,
               train.Direction,
               k = 3)

# the overly flexible fit yields bad results
table(knn.pred, data.test$Direction)
(48+87) / 2.52


