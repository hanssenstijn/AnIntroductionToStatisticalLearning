# remove all objects at once
rm(list=ls())

#library
library(ISLR)

# load data
data("Smarket")

# inspect data
names(Smarket)
dim(Smarket )
summary (Smarket )

pairs(Smarket )
cor(Smarket )
# remove last, which is categorical
cor(Smarket [,-9])

attach (Smarket )

#plot
plot(Volume)

# logistic regression
# glm() generalized linear models, class of models that includes logsitic regression
# family = binomial
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket ,family =binomial )
summary (glm.fit )

# get coeficient in different ways
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

# predict
# type="response" --> output probabilities of the form P(Y = 1|X),
glm.probs =predict(glm.fit,type ="response")
glm.probs[1:10]

# We know that these values correspond to the probability of the market going up, rather than down, because the
# contrasts() function indicates that R has created a dummy variable with a 1 for Up.
contrasts(Direction)

# convert predicton into up or down class
# The first command creates a vector of 1,250 Down elements. The second line
# transforms to Up all of the elements for which the predicted probability of a
# market increase exceeds 0.5.
glm.pred=rep ("Down " ,1250)
glm.pred[glm.probs >.5]=" Up"

# confusion matrix
table(glm.pred ,Direction)
# correct predictions
(507+145) /1250

# training test set
# boolean (True or False) can be used to obtain a subset of the rows/columns of a matrix
train =(Year <2005)
Smarket.2005= Smarket[!train,]
dim(Smarket.2005)
Direction.2005= Direction[!train]

# use subset of data to train model
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket ,family =binomial ,subset =train )
glm.probs =predict(glm.fit ,Smarket.2005, type="response")
glm.pred=rep ("Down " ,252)
glm.pred[glm.probs >.5]=" Up"
table(glm.pred ,Direction.2005)

# use only predictors which have highest predictive power
glm.fit=glm(Direction~Lag1+Lag2 ,data=Smarket ,family =binomial ,
            subset =train)
glm.probs =predict(glm.fit ,Smarket.2005 , type="response")
glm.pred=rep ("Down" ,252)
glm.pred[glm.probs >.5]=" Up"
table(glm.pred ,Direction.2005)

# Suppose that we want to predict the returns associated with particular
# values of Lag1 and Lag2.
predict(glm.fit ,newdata =data.frame(Lag1=c(1.2 ,1.5) ,
                                      Lag2=c(1.1 , -0.8) ),type ="response")

# Linear Discriminant Analysis
# lda() function we need the MASS package
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset =train)
# 49.2% of the training observations correspond to days during which the market went down.
lda.fit
# It also provides the group means; these are the average of each predictor within each class,
# If ???0.642×Lag1???0.514×Lag2 is large, then the LDA classifier will
# predict a market increase, and if it is small, then the LDA classifier will
# predict a market decline.
lda.pred=predict(lda.fit , Smarket.2005)
names(lda.pred)
lda.class =lda.pred$class
table(lda.class ,Direction.2005)
# using other threshold
sum(lda.pred$posterior [,1]>.4)

# Quadratic discriminant analysis
qda.fit=qda(Direction~Lag1+Lag2 ,data=Smarket ,subset =train)
# But it does not contain the coefficients
# of the linear discriminants, because the QDA classifier involves a
# quadratic, rather than a linear, function of the predictors.
qda.fit
qda.class =predict(qda.fit ,Smarket.2005)$class
table(qda.class ,Direction.2005)
(30+121) / 252

# K-nearest Neighbors (class library)
library(class)

# train, test and class labels
train.X=cbind(Lag1 ,Lag2)[train ,]
test.X=cbind (Lag1 ,Lag2)[!train ,]
train.Direction =Direction[train]

set.seed (1)
knn.pred=knn(train.X,test.X,train.Direction ,k=1)
table(knn.pred ,Direction.2005)
(83+43) /252

knn.pred=knn(train.X,test.X,train.Direction ,k=3)
table(knn.pred ,Direction.2005)
mean(knn.pred== Direction.2005)

# An application to caravan insurance data
library(ISLR)
data("Caravan")
dim(Caravan )
attach(Caravan)
summary(Purchase)
348/5822

# standardize data so that all variables are given a mean of zero and std of 1
# scale()
standardized.X=scale(Caravan[,-86])
var(Caravan [,1])
var(Caravan [,2])
var( standardized.X[,1])
var( standardized.X[,2])

# test train
test =1:1000
train.X=standardized.X[-test ,]
test.X=standardized.X[test ,]
train.Y=Purchase [-test]
test.Y=Purchase [test]
set.seed (1)
# knn = 1
knn.pred=knn (train.X,test.X,train.Y,k=1)
mean(test.Y!= knn.pred)
mean(test.Y!="No")
table(knn.pred ,test.Y)
#knn = 3
knn.pred=knn (train.X,test.X,train.Y,k=3)
table(knn.pred ,test.Y)

# Aplied chapter 4
# remove all objects at once
rm(list=ls())

# Q10

#library
library(ISLR)
library(corrplot)
#data
data("Weekly")

# numerical/graphical summaries
summary(Weekly)
pairs(Weekly)
cor(Weekly[,-9])
M=cor(subset(Weekly, select=-Direction))
corrplot(M,method = "circle", type="upper")

attach(Weekly)
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, 
              family = binomial)
summary(glm.fit)

#predict
glm.probs = predict(glm.fit, type = "response")
# confusion matrix
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction)

glm.fit = glm(Direction ~ Lag2, data = Weekly, 
              family = binomial)
summary(glm.fit)

# predict on 2009 and 2010
train = (Year < 2009)
# save all rows which are true
Weekly.0910 = Weekly[!train, ]
# model using only Lag2
glm.fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
# predict for new data
glm.probs = predict(glm.fit, Weekly.0910, type = "response")
# add collumn with "Down"
glm.pred = rep("Down", length(glm.probs))
# All which have higher prediction than 0.5 will get "Up" instead of "Down"
glm.pred[glm.probs > 0.5] = "Up"
# save all rows which are in training data
Direction.0910 = Direction[!train]
# confusion matrix
table(glm.pred, Direction.0910)
# fraction of correct predictions
mean(glm.pred == Direction.0910)

# library for lda
library(MASS)
lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.pred = predict(lda.fit, Weekly.0910)
table(lda.pred$class, Direction.0910)
mean(lda.pred$class == Direction.0910)

# QDA
qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.class = predict(qda.fit, Weekly.0910)$class
table(qda.class, Direction.0910)
mean(qda.class == Direction.0910)

# library KNN
library(class)
# set training x
train.X = as.matrix(Lag2[train])
# set test x
test.X = as.matrix(Lag2[!train])
# set prediction y
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)

# remove all objects at once
rm(list=ls())

# Q11

#library
library(ISLR)
library(corrplot)
#data
data("Auto")

# summary
summary(Auto)

attach(Auto)
# create binary variable "mpg01" 0 is lower than median, 1 if its higher
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
# combine the new variable to the auto dataset
Auto = data.frame(Auto, mpg01)

cor(Auto[, -9])
M=cor(subset(Auto, select=-name))
corrplot(M,method = "circle", type="upper")
# doesn't work well since mpg01 is 0 or 1
pairs(Auto)

# split data
# if the year is even
train = (year%%2 == 0) 
test = !train
Auto.train = Auto[train, ]
Auto.test = Auto[test, ]
mpg01.test = mpg01[test]

# LDA
library(MASS)
lda.fit = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
lda.pred = predict(lda.fit, Auto.test)
# 12.6% test error rate.
mean(lda.pred$class != mpg01.test)

# QDA
qda.fit = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
qda.pred = predict(qda.fit, Auto.test)
mean(qda.pred$class != mpg01.test)

# Logistic regression
# subset is train
glm.fit = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              family = binomial, subset = train)
# type is response for logisitc regression
glm.probs = predict(glm.fit, Auto.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != mpg01.test)

# KNN
library(class)
train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[test, ]
train.mpg01 = mpg01[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k = 1)
mean(knn.pred != mpg01.test)

# remove all objects at once
rm(list=ls())

# Q12
Power = function() {
    2^3
}
print(Power())

Power2 = function(x, a) {
    x^a
}
Power2(3, 8)

Power2(10, 3)

Power3 = function(x, a) {
    result = x^a
    return(result)
}

x = 1:10
plot(x, Power3(x, 2), log = "xy", ylab = "Log of y = x^2", xlab = "Log of x", 
     main = "Log of x^2 versus Log of x")
