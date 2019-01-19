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
