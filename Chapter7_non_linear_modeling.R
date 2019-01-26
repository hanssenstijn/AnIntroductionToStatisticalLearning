# remove all objects at once
rm(list=ls())

# load lib
library(ISLR)
attach(Wage)

# fit linear model
# use fourth-degree polynomial
# The function returns a matrix whose columns are a basis of orthogonal
#polynomials, which essentially means that each column is a linear orthogonal
# combination of the variables age, age^2, age^3 and age^4.

fit=lm(wage~poly(age ,4) ,data=Wage)
coef(summary (fit))

# We can do this by using the raw=TRUE argument to the poly() function.
fit2=lm(wage~poly(age ,4, raw =T),data=Wage)
coef(summary (fit2))

# an other way
fit2a=lm(wage~age+I(age ^2)+I(age ^3)+I(age ^4) ,data=Wage)
coef(fit2a)

# We now create a grid of values for age at which we want predictions, and
# then call the generic predict() function, specifying that we want standard
# errors as well.
agelims =range(age)
age.grid=seq (from=agelims [1], to=agelims [2])
preds=predict (fit ,newdata =list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)

# we plot the data and add the fit from the degree-4 polynomial.
# Here the mar and oma arguments to par() allow us to control the margins
# of the plot, and the title() function creates a figure title that spans both title() subplots.
par(mfrow =c(1,2) ,mar=c(4.5 ,4.5 ,1 ,1) ,oma=c(0,0,4,0))
plot(age ,wage ,xlim=agelims ,cex =.5, col =" darkgrey ")
title (" Degree -4 Polynomial ",outer =T)
lines(age.grid ,preds$fit ,lwd =2, col =" blue")
matlines (age.grid ,se.bands ,lwd =1, col =" blue",lty =3)

# ANOVA
fit.1= lm(wage~education +age ,data=Wage)
fit.2= lm(wage~education +poly(age ,2) ,data=Wage)
fit.3= lm(wage~education +poly(age ,3) ,data=Wage)
anova(fit.1, fit.2, fit.3)

# Splines
install.packages("splines2")
library(splines)
# we saw that regression splines can be fit by constructing an appropriate matrix of basis functions.
# The bs() function generates the entire matrix of bs()
# basis functions for splines with the specified set of knots.
# By default, cubic splines are produced.
# Fitting wage to age using a regression spline is simple:
fit=lm(wage~bs(age ,knots =c(25 ,40 ,60) ),data=Wage)
pred=predict(fit ,newdata =list(age =age.grid),se=T)
plot(age ,wage ,col =" gray ")
lines(age.grid ,pred$fit ,lwd =2)
lines(age.grid ,pred$fit +2* pred$se ,lty ="dashed")
lines(age.grid ,pred$fit -2* pred$se ,lty ="dashed")
