# remove all objects at once
rm(list=ls())

#install package
#install.packages("tree")

# lib
library(tree)
library (ISLR)

attach(Carseats)
# which takes on a value of Yes if the Sales variable exceeds 8, and
# takes on a value of No otherwise.
High=ifelse(Sales <=8,"No","Yes")
