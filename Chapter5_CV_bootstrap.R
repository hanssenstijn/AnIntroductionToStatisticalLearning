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
lm.fit2=lm(mpg~poly(horsepower ,2) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)
lm.fit3=lm(mpg~poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)

# different training set give other MSE values
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset = train)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)

# Leave one out cross validation

# But if we use glm() to fit a model without passing in the family argument, then it performs linear regression,
# just like the lm() function. So for instance:
glm.fit=glm(mpg~horsepower ,data=Auto)
coef(glm.fit)
lm.fit =lm(mpg~horsepower ,data=Auto)
coef(lm.fit)

# the cv.glm() function is part of the boot library
library(boot)
glm.fit=glm(mpg~horsepower ,data=Auto)
cv.err =cv.glm(Auto ,glm.fit)
# the delta vector contain the cross-validation results
# the two numbers are similar
cv.err$delta
# when are the numbers not similar
# for loop: for() i=1 to i=5, for(i in 1:5){ }
# store in cv.error
cv.error=rep (0,5)
for (i in 1:5){
    glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
    cv.error[i]=cv.glm (Auto ,glm.fit)$delta [1]
}
cv.error

# k-Fold Cross-Validation
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10) {
    glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
    cv.error.10[i]=cv.glm (Auto ,glm.fit ,K=10) $delta [1]
}
cv.error.10

# The bootstrap
# two step process
# function that computes the statistic of interest
# use boot() function, which is part of the boot library, to perform the bootstrap by repeatedly sampling observations from the data set with replacements
# first create a function, alpha.fn(), which takes as input the (X, Y) data as well as
# a vector indicating which observations should be used to estimate ??. The
#function then outputs the estimate for ?? based on the selected observations.
alpha.fn=function (data ,index){
    X=data$X [index]
    Y=data$Y [index]
    return ((var(Y)-cov (X,Y))/(var(X)+var(Y) -2* cov(X,Y)))
}

alpha.fn(Portfolio ,1:100)

# radmoly select 100 observations with replacement
set.seed(1)
alpha.fn(Portfolio, sample(100,100,replace = T))

# boot automates this process
boot(Portfolio, alpha.fn, R=1000)

# The bootstrap approach can be used to assess the variability of the coefficient
#estimates and predictions from a statistical learning method.
# here acess the variability of the estimates for beta0 and beta1


# Exercises
#Q2
# Create a plot that displays, for each integer value of n from 1
# to 100, 000, the probability that the jth observation is in the bootstrap sample.
pr = function(n) return(1 - (1 - 1/n)^n)
x = 1:1e+05
plot(x, pr(x))

# Applied section
#Q5
library(ISLR)
summary(Default)
set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)

FiveB = function() {
    # i.
    train = sample(dim(Default)[1], dim(Default)[1]/2)
    # ii.
    glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                  subset = train)
    # iii.
    glm.pred = rep("No", dim(Default)[1]/2)
    glm.probs = predict(glm.fit, Default[-train, ], type = "response")
    glm.pred[glm.probs > 0.5] = "Yes"
    # iv.
    return(mean(glm.pred != Default[-train, ]$default))
}
FiveB()
FiveB()
FiveB()

train = sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, 
              subset = train)
glm.pred = rep("No", dim(Default)[1]/2)
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)

#Q6
library(ISLR)
summary(Default)
attach(Default)

set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)

boot.fn = function(data, index) 
    return(coef(glm(default ~ income + balance, 
        data = data, family = binomial, subset = index)))

library(boot)
boot(Default, boot.fn, 50)

#Q7
library(ISLR)
summary(Weekly)
set.seed(1)
attach(Weekly)
glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(glm.fit)
# leave out the first observation
glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-1, ], family = binomial)
summary(glm.fit)
# predict. predicts true;UP but it was actually down!
predict.glm(glm.fit, Weekly[1, ], type = "response") > 0.5

count = rep(0, dim(Weekly)[1])
for (i in 1:(dim(Weekly)[1])) {
    glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ], family = binomial)
    is_up = predict.glm(glm.fit, Weekly[i, ], type = "response") > 0.5
    is_true_up = Weekly[i, ]$Direction == "Up"
    if (is_up != is_true_up) 
        count[i] = 1
}
# how many errors
sum(count)
# error rate
mean(count)

#Q8
# generate dataset
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)
plot(x, y)

library(boot)
Data = data.frame(x, y)
set.seed(1)
# first least sqaures model
glm.fit = glm(y ~ x)
cv.glm(Data, glm.fit)$delta
# poly model
glm.fit = glm(y ~ poly(x, 2))
cv.glm(Data, glm.fit)$delta
# cubic model
glm.fit = glm(y ~ poly(x, 3))
cv.glm(Data, glm.fit)$delta
# ^4
glm.fit = glm(y ~ poly(x, 4))
cv.glm(Data, glm.fit)$delta
# The quadratic polynomial had the lowest LOOCV test error rate. This was expected because it matches the true form of Y.
summary(glm.fit)

#Q9
library(MASS)
summary(Boston)
set.seed(1)
attach(Boston)
medv.mean = mean(medv)
medv.mean
medv.err = sd(medv)/sqrt(length(medv))
medv.err

# using boostrap
boot.fn = function(data, index) return(mean(data[index]))
library(boot)
bstrap = boot(medv, boot.fn, 1000)
bstrap

# t.test
t.test(medv)
# bootstrap
c(bstrap$t0 - 2 * 0.4119, bstrap$t0 + 2 * 0.4119)

medv.med = median(medv)
medv.med
# bootstrap median
boot.fn = function(data, index) return(median(data[index]))
boot(medv, boot.fn, 1000)

medv.tenth = quantile(medv, c(0.1))
medv.tenth
# bootstrap quantile
boot.fn = function(data, index) return(quantile(data[index], c(0.1)))
boot(medv, boot.fn, 1000)
