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

