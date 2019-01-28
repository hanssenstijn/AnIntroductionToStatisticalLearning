# remove all objects at once
rm(list=ls())

# lib
library(e1071)

# the svm() function can be used to fit a
# support vector classifier when the argument kernel="linear" is used.
# A cost argument allows us to specify the cost of a violation to the margin. When the cost argument is small, then the margins
# will be wide and many support vectors will be on the margin or will violate the margin.
set.seed(1)
x=matrix(rnorm (20*2) , ncol =2)
y=c(rep(-1,10), rep(1 ,10))
x[y==1,]= x[y==1,] + 1
plot(x, col =(3-y))
dat=data.frame(x=x, y=as.factor(y))
svmfit =svm(y~., data=dat , kernel ="linear", cost =10,
            scale =FALSE)
# The argument scale=FALSE tells the svm() function not to scale each feature
# to have mean zero or standard deviation one; depending on the application, one might prefer to use scale=TRUE.
plot(svmfit , dat)
# determine identities support vectors
svmfit$index
summary(svmfit)
# using lower cost
svmfit =svm(y~., data=dat , kernel ="linear", cost =0.1,
            scale =FALSE )
plot(svmfit , dat)
svmfit$index

# tune(), to perform cross-validation.
set.seed(1)
tune.out=tune(svm ,y~.,data=dat ,kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
# get best model
bestmod =tune.out$best.model
summary(bestmod)

# predict()
xtest=matrix(rnorm (20*2) , ncol =2)
ytest=sample(c(-1,1) , 20, rep=TRUE)
xtest[ytest ==1 ,]= xtest[ytest ==1,] + 1
testdat =data.frame(x=xtest , y=as.factor(ytest))
ypred=predict(bestmod ,testdat)
table(predict =ypred , truth= testdat$y)

# what if we change the cost value
svmfit =svm(y~., data=dat , kernel ="linear", cost =.01,
            scale =FALSE )
ypred=predict (svmfit ,testdat)
table(predict =ypred , truth= testdat$y)

# svm()
x[y==1 ,]= x[y==1 ,]+0.5
plot(x, col =(y+5) /2, pch =19)
# large value of cost
dat=data.frame(x=x,y=as.factor(y))
svmfit =svm(y~., data=dat , kernel ="linear", cost =1e5)
summary (svmfit )
plot(svmfit , dat)

# To fit an SVM with a polynomial kernel we use kernel="polynomial", and
# to fit an SVM with a radial kernel we use kernel="radial".
set.seed(1)
x=matrix(rnorm(200*2) , ncol =2)
x[1:100 ,]=x[1:100 ,]+2
x[101:150 ,]= x[101:150 ,] -2
y=c(rep(1 ,150) ,rep(2 ,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=y)

# random split
train=sample(200 ,100)
svmfit =svm(y~., data=dat[train ,], kernel ="radial", gamma =1,
            cost =1)
plot(svmfit , dat[train ,])
summary(svmfit)

# tune CV
set.seed(1)
tune.out=tune(svm , y~., data=dat[train ,], kernel ="radial",
              ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                           gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train ,"y"], pred=predict(tune.out$best.model ,
                                           newx=dat[-train ,]))


# ROC curves
library(ROCR)
# Function to plot an ROC curve
#given a vector containing a numerical score for each observation, pred, and
#a vector containing the class label for each observation, truth.
rocplot =function (pred , truth , ...){
    predob = prediction (pred , truth )
    perf = performance (predob , "tpr", "fpr")
    plot(perf ,...)}
# if the fitted value exceeds zero then the observation
# is assigned to one class, and if it is less than zero than it is assigned to the other.
# In order to obtain the fitted values for a given SVM model fit, we
# use decision.values=TRUE when fitting svm().
svmfit.opt=svm(y~., data=dat[train ,], kernel ="radial",
                gamma =2, cost=1, decision.values =T)
fitted =attributes (predict (svmfit.opt ,dat[train ,], decision.values =TRUE))$decision.values
par(mfrow =c(1,2))
rocplot(fitted ,dat [train ,"y"], main=" Training Data")

# SVM with multiple classes
set.seed(1)
x=rbind(x, matrix (rnorm (50*2) , ncol =2))
y=c(y, rep (0 ,50) )
x[y==0 ,2]= x[y==0 ,2]+2
dat=data.frame(x=x, y=as.factor (y))
par(mfrow =c(1,1))
plot(x,col =(y+1))
svmfit =svm(y~., data=dat , kernel ="radial", cost =10, gamma =1)
plot(svmfit , dat)

# application to gene expression data
library(ISLR)
names(Khan)
dim( Khan$xtrain)
dim( Khan$xtest)
table(Khan$ytrain)
table(Khan$ytest)
# training data
dat=data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain))
out=svm(y~., data=dat , kernel ="linear",cost =10)
summary(out)
table(out$fitted , dat$y)

# test data
dat.te=data.frame(x=Khan$xtest , y=as.factor(Khan$ytest))
pred.te=predict(out , newdata =dat.te)
table(pred.te , dat.te$y)

# Excercises
# Q3.
x1 = c(3, 2, 4, 1, 2, 4, 4)
x2 = c(4, 2, 4, 4, 1, 3, 1)
colors = c("red", "red", "red", "red", "blue", "blue", "blue")
plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.5, 1)
abline(-1, 1, lty = 2)
abline(0, 1, lty = 2)
arrows(2, 1, 2, 1.5)
arrows(2, 2, 2, 1.5)
arrows(4, 4, 4, 3.5)
arrows(4, 3, 4, 3.5)

#Q7
library(ISLR)
gas.med = median(Auto$mpg)
# if else higher than median
new.var = ifelse(Auto$mpg > gas.med, 1, 0)
Auto$mpglevel = as.factor(new.var)

library(e1071)
# using various cost values
set.seed(3255)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 
                                                                                  0.1, 1, 5, 10, 100)))
# We see that cross-validation error is minimized for cost=1.
summary(tune.out)

# Q8
library(ISLR)
set.seed(9004)
# create training and test set
train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = 0.01)
summary(svm.linear)
# predict training error
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
# test error
test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)

# tune function to select an optimal cost
set.seed(1554)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "linear", ranges = list(cost = 10^seq(-2, 
                                                                                                   1, by = 0.25)))
summary(tune.out)

# using best tune value
svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)

