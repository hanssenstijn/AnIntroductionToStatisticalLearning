# install packages which contain the datasets
install.packages("ISLR")
install.packages("MASS")

# set library
library(ISLR)
library(MASS)

x <- c(1,3,2,5)
x
x = c(1,6,2)
x
y = c(1,4,3)
length(x)
length(y)
x+y
ls()
rm(x,y)
ls()
# remove all objects at once
rm(list=ls())
x=matrix (data=c(1,2,3,4) , nrow=2, ncol =2)
x
x=matrix (c(1,2,3,4) ,2,2)
x
matrix (c(1,2,3,4) ,2,2,byrow =TRUE)
sqrt(x)
x^2
x = rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)
# reproduce the results
set.seed (1303)
rnorm(50)
set.seed (3)
y=rnorm (100)
mean(y)
var(y)

# Graphics
x=rnorm(100)
y=rnorm(100)
plot(x,y)

# Sequence
seq(0,1,length=10)

# Contour plot three-dimensional data
x=seq(-pi ,pi ,length =50)
y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
# color-coded plot
fa=(f-t(f))/2
image(x,y,fa)

# Indexing data
A=matrix (1:16 ,4 ,4)
A
A[2,3]
A[c(1,3) ,c(2,4) ]
A[1,]
dim(A)

# load data
data("Auto")
# view as spreadsheet
fix(Auto)
names(Auto)

plot(Auto$cylinders , Auto$mpg )
attach(Auto)
plot(cylinders , mpg)
cylinders =as.factor (cylinders )
plot(cylinders , mpg , col ="red", varwidth =T, xlab=" cylinders ",
     ylab ="MPG ")
hist(mpg ,col =2, breaks =15)

plot(horsepower ,mpg)
#identifying the value for a particular variable for points on a plot
identify (horsepower ,mpg ,name)

#save a record of all of the commands that we typed in the most recent session
savehistory()

