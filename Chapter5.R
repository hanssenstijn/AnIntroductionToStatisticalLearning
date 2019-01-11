# lab cross validation and the bootstrap

# remove all objects at once
rm(list=ls())

# load lib
library (ISLR)
#load data
data("Auto")

set.seed(1)

# It is generally a good idea to set a random seed when performing an analysis such as cross-validation
# that contains an element of randomness, so that the results obtained can be reproduced precisely at a later time.

# select random subset of 196 obs out of the orignial 392
train=sample(392,196)
# train model using only the train set
lm.fit =lm(mpg~horsepower,data=Auto,subset =train )

attach(Auto)
# mean function to calculate the MSE of the validation set
mean((mpg -predict(lm.fit ,Auto))[-train ]^2)
# poly() to estimate the test error for the polynomial and cubic regression
