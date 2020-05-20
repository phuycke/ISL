# --------------------------------------------- #
# Introduction to Statistical Learning          #
# Chapter 02: Statistical Learning (p. 28 - 71) #
# Applied exercises: exercise 10                #
# --------------------------------------------- #

rm(list = ls())

library(MASS)
fix(Boston)

# 10 b)
pairs(Boston)

# 10 d)
summary(Boston$crim)
summary(Boston$tax)
summary(Boston$ptratio)

# 10 e)
table(Boston$chas)

# 10 f)
median(Boston$ptratio)

# 10 g)
subset = Boston[Boston$medv == min(Boston$medv),]

# 10 h)
length(Boston[Boston$rm > 7,])
length(Boston[Boston$rm > 8,])

# subset the dataset, and compare it with the rest of the data
subset = Boston[Boston$rm > 8,]
rest   = Boston[!row.names(Boston) %in% row.names(subset),]

summary(subset)
summary(rest)
