# --------------------------------------------- #
# Introduction to Statistical Learning          #
# Chapter 02: Statistical Learning (p. 28 - 71) #
# Applied exercises: exercise 09                #
# --------------------------------------------- #

# 9 a)
setwd("C:/Users/pieter/Downloads/Books/Statistics/General statistics/ISL/Datasets")
auto = read.csv("Auto.csv", header = T)
summary(auto)

# 9 c)
mean(auto$mpg)
sd(auto$mpg)

# 9 d)
auto_sub = auto[-c(10:85),]
summary(auto_sub)

# 9 e)
pairs(auto)
