# remove all objects at once
rm(list=ls())

# install package
install.packages("car")
# set library
library(ISLR)
library(MASS)
library(car)

# load data
data("Boston")
?Boston
fix(Boston)
names(Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict (lm.fit ,data.frame(lstat =(c(5 ,10 ,15) )),
         interval ="confidence")
predict (lm.fit ,data.frame(lstat =(c(5 ,10 ,15) )),
         interval ="prediction")
plot(lstat ,medv ,pch ="+")
abline (lm.fit ,lwd =3, col ="red ")
# plot the lm.fit, get all four figures at one showing. 
par(mfrow =c(2,2))
plot(lm.fit)
# stop the par mfrow
dev.off()
plot(hatvalues (lm.fit ))

# Multiple linear regression
lm.fit =lm(medv~lstat+age ,data=Boston )
summary (lm.fit)

lm.fit =lm(medv~. ,data=Boston )
summary (lm.fit)

vif(lm.fit)

# use all but one variable : y~.-x
lm.fit1=lm(medv~.-age,data=Boston)

# interaction terms
summary(lm(medv~lstat*age ,data=Boston ))

# non-linear transformations of the predictors
# I(X^2) --> the I() is need since "^" has speacial meaning in a formula
lm.fit2=lm(medv~lstat +I(lstat ^2))
summary (lm.fit2)

# anova to further quantify the extent to which the quadratic fit is superior
anova(lm.fit ,lm.fit2)

par(mfrow =c(2,2))
plot(lm.fit2)
dev.off()

lm.fit5=lm(medv~poly(lstat ,5))
summary (lm.fit5)

# Qualitative predictors
data("Carseats")
fix(Carseats)
names(Carseats)

# generates dummy variables automatically.
lm.fit =lm(Sales~.+ Income :Advertising +Price :Age ,data=Carseats )
summary (lm.fit)

attach(Carseats)
# return coding dummy variables : contrasts()
contrasts(ShelveLoc)
?contrasts

# Writing functions
LoadLibraries=function (){
    library (ISLR)
    library (MASS)
    print (" The libraries have been loaded .")
}
LoadLibraries()

# Applied
# Q 8
data("Auto")
attach(Auto)
lm.fit=lm(mpg~horsepower)
summary(lm.fit)
dev.off()
plot(horsepower ,mpg ,pch ="+")
abline (lm.fit ,lwd =3, col ="red ")
# What is the predicted mpg associated with a horsepower of 98?
predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")

par(mfrow =c(2,2))
plot(lm.fit)
dev.off()
