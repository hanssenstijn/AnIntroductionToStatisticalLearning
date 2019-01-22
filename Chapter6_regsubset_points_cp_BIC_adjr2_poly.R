# remove all objects at once
rm(list=ls())
# load lib
library (ISLR)
library (leaps)
# check data
fix(Hitters)
# columns
names(Hitters )
dim(Hitters )
# how many missing
sum(is.na(Hitters$Salary))
# remove missing
Hitters =na.omit(Hitters )
dim(Hitters )
sum(is.na(Hitters ))

# The regsubsets() function (part of the leaps library) performs best subregsubsets()
#set selection by identifying the best model that contains a given number
#of predictors, where best is quantified using RSS.
regfit.full=regsubsets (Salary~.,Hitters )
#An asterisk indicates that a given variable is included in the corresponding model.
summary (regfit.full)
# By default, regsubsets() only reports results up to the best eight-variable model.
# increase number
regfit.full=regsubsets (Salary~.,data=Hitters ,nvmax =19)
reg.summary =summary(regfit.full)
names(reg.summary )
# R^2 improvement when adding variables
reg.summary$rsq
# plot RSS,R^2, Cp and BIC
# Note the type="l" option tells R to connect the plotted points with lines.
par(mfrow =c(2,2))
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",
     type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",
     ylab=" Adjusted RSq",type="l")
# The points() command works like the plot() command, except that it points()
# puts points on a plot that has already been created, instead of creating a new plot.
# The which.max() function can be used to identify the location of the maximum point of a vector.
which.max (reg.summary$adjr2)
points (11, reg.summary$adjr2[11], col ="red",cex =2, pch =20)
plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp",
     type="l")
which.min (reg.summary$cp )
points (10, reg.summary$cp [10], col ="red",cex =2, pch =20)
plot(reg.summary$bic ,xlab=" Number of Variables ",ylab=" BIC",
     type="l")
which.min (reg.summary$bic )
points (6, reg.summary$bic [6], col =" red",cex =2, pch =20)
# The regsubsets() function has a built-in plot() command which can
# be used to display the selected variables for the best model with a given
# number of predictors, ranked according to the BIC, Cp, adjusted R2, or AIC.
plot(regfit.full ,scale ="r2")
plot(regfit.full ,scale ="adjr2")
plot(regfit.full ,scale ="Cp")
plot(regfit.full ,scale ="bic")
coef(regfit.full ,6)
# Forward and Backward Stepwise Selection
regfit.fwd=regsubsets (Salary~.,data=Hitters ,nvmax =19,
                        method ="forward")
# Choosing Among Models Using the Validation Set Approach and Cross-Validation
set.seed(1)
train=sample(c(TRUE ,FALSE), nrow(Hitters),rep=TRUE)
test =(!train)
regfit.best=regsubsets (Salary~.,data=Hitters[train,],
                         nvmax =19)
test.mat=model.matrix (Salary~.,data=Hitters [test,])
val.errors =rep(NA ,19)
for(i in 1:19){
    coefi=coef(regfit.best ,id=i)
    pred=test.mat [,names(coefi)]%*% coefi
    val.errors [i]= mean(( Hitters$Salary[test]-pred)^2)
}
val.errors
which.min (val.errors)
coef(regfit.best ,10)
# write own prediction function
predict.regsubsets =function (object ,newdata ,id ,...){
    form=as.formula(object$call[[2]])
    mat=model.matrix (form ,newdata )
    coefi =coef(object,id=id)
    xvars =names(coefi )
    mat[,xvars ]%*% coefi
}
regfit.best=regsubsets(Salary~.,data=Hitters ,nvmax =19)    
# cross validation
k=10
set.seed(1)
folds=sample (1:k,nrow(Hitters ),replace =TRUE)
# set matrix for : given us a 10×19 matrix, of which the (i, j)th element corresponds
# to the test MSE for the ith cross-validation fold for the best j-variable model.
cv.errors =matrix (NA ,k,19, dimnames =list(NULL , paste (1:19) ))
# for loop for cross-validation
# In the jth fold, the elements of folds that equal j are in the test set, and the remainder are in the training set.
for(j in 1:k){
    best.fit =regsubsets(Salary~.,data=Hitters[folds !=j,],
                          nvmax =19)
for(i in 1:19) { 
    pred=predict(best.fit ,Hitters[folds ==j,], id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds ==j]-pred)^2)
}
}
mean.cv.errors =apply(cv.errors ,2, mean)
mean.cv.errors
par(mfrow =c(1,1))
plot(mean.cv.errors ,type="b")
# CV selects an 11 variable model
reg.best=regsubsets(Salary~.,data=Hitters , nvmax =19)
coef(reg.best ,11)

#Ridge Regression and the Lasso
library(glmnet)
# The model.matrix() function is particularly useful for creating x; not only
#does it produce a matrix corresponding to the 19 predictors but it also
#automatically transforms any qualitative variables into dummy variables.
x=model.matrix (Salary~.,Hitters)[,-1]
y=Hitters$Salary

# Ridge Regression
# If alpha=0 then a ridge regression model is fit, and if alpha=1 then a lasso model is fit.
grid =10^ seq (10,-2, length =100)
ridge.mod =glmnet (x,y,alpha =0, lambda =grid)
# Note that by default, the glmnet() function standardizes the
# variables so that they are on the same scale.
dim(coef(ridge.mod ))
# These are the coefficients when ?? = 11,498
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[ -1 ,50]^2))
# For instance, we can obtain the ridge regression coefficients for a new value of ??, say 50:
predict(ridge.mod ,s=50, type ="coefficients")[1:20 ,]

# example
set.seed(1)
train=sample (1: nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod =glmnet(x[train ,],y[train],alpha =0, lambda =grid ,
                   thresh =1e-12)
ridge.pred=predict(ridge.mod ,s=4, newx=x[test ,])
mean(( ridge.pred -y.test)^2)
mean(( mean(y[train ])-y.test)^2)
lm(y~x, subset =train)

# The lasso
lasso.mod =glmnet(x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)
# We now perform cross-validation and compute the associated test error.
set.seed (1)
cv.out =cv.glmnet(x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred=predict(lasso.mod ,s=bestlam ,newx=x[test ,])
mean((lasso.pred -y.test)^2)
out=glmnet(x,y,alpha =1, lambda =grid)
lasso.coef=predict(out ,type ="coefficients",s=bestlam )[1:20 ,]
lasso.coef

# PCR and PLS Regression
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters ,scale=TRUE ,
            validation ="CV")
# Note that pcr() reports the root mean squared error; in order to obtain
# the usual MSE, we must square this quantity
summary(pcr.fit)
validationplot(pcr.fit ,val.type="MSEP")

# Exercises
# applied section
# Q8
# Create 100 X and ?? variables
set.seed(1)
X = rnorm(100)
eps = rnorm(100)

# We are selecting ??0=3, ??1=2, ??2=???3 and ??3=0.3.
beta0 = 3
beta1 = 2
beta2 = -3
beta3 = 0.3
Y = beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps

# Use regsubsets to select best model having polynomial of X of degree 10
library(leaps)
data.full = data.frame(y = Y, x = X)
mod.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
mod.summary = summary(mod.full)
# Find the model size for best cp, BIC and adjr2
which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
# Plot cp, BIC and adjr2
plot(mod.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(3, mod.summary$cp[3], pch = 4, col = "red", lwd = 7)
plot(mod.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(3, mod.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(mod.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, 
     type = "l")
points(3, mod.summary$adjr2[3], pch = 4, col = "red", lwd = 7)
# check coeficients for 3
coefficients(mod.full, id = 3)

# Training data on lasso
library(glmnet)
xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data.full)[, -1]
mod.lasso = cv.glmnet(xmat, Y, alpha = 1)
best.lambda = mod.lasso$lambda.min
best.lambda
plot(mod.lasso)
# Next fit the model on entire data using best lambda
best.model = glmnet(xmat, Y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

# Q9
# Load and split the College data
library(ISLR)
set.seed(11)
sum(is.na(College))
train.size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train.size)
test = -train
College.train = College[train, ]
College.test = College[test, ]

# NUmber of applications is the Apps variable.
lm.fit = lm(Apps~., data=College.train)
lm.pred = predict(lm.fit, College.test)
mean((College.test[, "Apps"] - lm.pred)^2)

# Ridge
train.mat = model.matrix(Apps~., data=College.train)
test.mat = model.matrix(Apps~., data=College.test)
grid = 10 ^ seq(4, -2, length=100)
mod.ridge = cv.glmnet(train.mat, College.train[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best = mod.ridge$lambda.min
lambda.best
ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - ridge.pred)^2)

# PCR
library(pls)
pcr.fit = pcr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred = predict(pcr.fit, College.test, ncomp=10)
mean((College.test[, "Apps"] - data.frame(pcr.pred))^2)

#Q10
# test error
# create data
set.seed(1)
p = 20
n = 1000
x = matrix(rnorm(n * p), n, p)
B = rnorm(p)
B[3] = 0
B[4] = 0
B[9] = 0
B[19] = 0
B[10] = 0
eps = rnorm(p)
y = x %*% B + eps
# split
train = sample(seq(1000), 100, replace = FALSE)
y.train = y[train, ]
y.test = y[-train, ]
x.train = x[train, ]
x.test = x[-train, ]

# traing set 
library(leaps)
regfit.full = regsubsets(y ~ ., data = data.frame(x = x.train, y = y.train), 
                         nvmax = p)
val.errors = rep(NA, p)
x_cols = colnames(x, do.NULL = FALSE, prefix = "x.")
for (i in 1:p) {
    coefi = coef(regfit.full, id = i)
    pred = as.matrix(x.train[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% 
                                                                        x_cols]
    val.errors[i] = mean((y.train - pred)^2)
}
plot(val.errors, ylab = "Training MSE", pch = 19, type = "b")

# test set
val.errors = rep(NA, p)
for (i in 1:p) {
    coefi = coef(regfit.full, id = i)
    pred = as.matrix(x.test[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% 
                                                                       x_cols]
    val.errors[i] = mean((y.test - pred)^2)
}
plot(val.errors, ylab = "Test MSE", pch = 19, type = "b")
which.min(val.errors)
coef(regfit.full, id = 16)
