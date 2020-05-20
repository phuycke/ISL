# --------------------------------------------- #
# Introduction to Statistical Learning          #
# Chapter 02: Statistical Learning (p. 28 - 71) #
# Applied exercises: exercise 08                #
# --------------------------------------------- #

# 8 a)
setwd("C:/Users/pieter/Downloads/Books/Statistics/General statistics/ISL/Datasets")
college = read.csv("College.csv", header = T)

# 8 b)
rownames(college) = college$X
college$X         = NULL
fix(college)

# 8 c)
summary(college)
pairs(college[,1:10])
plot(college$Private, college$Outstate)

Elite   = rep ("No", nrow(college))
Elite[college$Top10perc >50] = " Yes"
Elite   = as.factor (Elite)
college = data.frame(college ,Elite)

summary(college$Elite)
plot(college$Elite, college$Outstate)

par(mfrow=c(2,2))
hist(college$Personal)
hist(college$Terminal)
hist(college$Books)
hist(college$Expend)

# 8 further data exploration
## Elite uni and money
par(mfrow=c(1,3))
plot(college$Elite, college$Expend)
plot(college$Elite, college$Books)
plot(college$Elite, college$Room.Board)

# Elite uni and the amount of people accepted
par(mfrow=c(1,3))
plot(college$Elite, college$Apps)
plot(college$Elite, college$Accept)
plot(college$Elite, college$Enroll)

# Successful students and (post) graduation
par(mfrow=c(1,2))
plot(college$Top10perc, college$Grad.Rate)
plot(college$Top10perc, college$Outstate)
