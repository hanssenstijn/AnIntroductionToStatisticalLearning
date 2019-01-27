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
# Finally, we use the data.frame() function to merge High with the rest of the Carseats data.
Carseats =data.frame(Carseats ,High)
# We now use the tree() function to fit a classification tree in order to predict tree()
# High using all variables but Sales.
tree.carseats =tree(High~.-Sales ,Carseats )
# The residual mean deviance reported is simply the deviance divided by n???|T0|, which in this case is 400???27 = 373.
summary(tree.carseats)
# The argument pretty=0 instructs R to include the category names for any qualitative predictors,
# rather than simply displaying a letter for each category.
plot(tree.carseats)
text(tree.carseats ,pretty =0)
# shows the split criterion, the number of observations in the brach, deviance, the overall prediction and fraction observation
tree.carseats
# In the case of a classification tree, the argument type="class" instructs R to return
# the actual class prediction.
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset = train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
# all right/true values
(86+57) /200

# consider whether pruning the tree might lead to improved results
# The function cv.tree() performs cross-validation in order to cv.tree()
# determine the optimal level of tree complexity; cost complexity pruning
# is used in order to select a sequence of trees for consideration.
# We use the argument FUN=prune.misclass in order to indicate that we want the
# classification error rate to guide the cross-validation and pruning process
# The cv.tree() function reports the number of terminal nodes of each tree considered
# (size) as well as the corresponding error rate and the value of the
# cost-complexity parameter used (k, which corresponds to ??.
set.seed(3)
cv.carseats = cv.tree(tree.carseats,FUN = prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow =c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")
# We now apply the prune.misclass() function in order to prune the tree to prune. obtain the nine-node tree.
prune.carseats =prune.misclass(tree.carseats ,best =9)
plot(prune.carseats)
text(prune.carseats ,pretty =0)
tree.pred=predict(prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)
(94+60) /200
# If we increase the value of best, we obtain a larger pruned tree with lower classification accuracy
prune.carseats =prune.misclass(tree.carseats ,best =15)
plot(prune.carseats)
text(prune.carseats ,pretty =0)
tree.pred=predict(prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)
(86+62) /200
