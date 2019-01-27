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

# fitting regression trees
library(MASS)
set.seed(1)
train = sample(1: nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset = train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty = 0)
#prunning
cv.boston =cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev ,type="b")
prune.boston =prune.tree(tree.boston ,best =5)
plot(prune.boston )
text(prune.boston ,pretty =0)
# In keeping with the cross-validation results, we use the unpruned tree to make predictions on the test set.
yhat=predict(tree.boston ,newdata =Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat,boston.test)
abline(0,1)
#calculate MSE --> sqaure root needed thus 5
mean((yhat -boston.test)^2)

# Bagging and random forest
library(randomForest)
set.seed(1)
# The argument mtry=13 indicates that all 13 predictors should be considered for each split of the tree
bag.boston =randomForest(medv~.,data=Boston ,subset =train ,
                         mtry=13, importance =TRUE)
bag.boston
yhat.bag = predict (bag.boston ,newdata =Boston[-train ,])
plot(yhat.bag , boston.test)
abline (0,1)
mean((yhat.bag -boston.test)^2)
# We could change the number of trees grown by randomForest() using the ntree argument
bag.boston =randomForest(medv~.,data=Boston ,subset =train ,
                         mtry=13, ntree =25)
# Growing a random forest proceeds in exactly the same way, except that
# we use a smaller value of the mtry argument.
rf.boston =randomForest(medv~.,data=Boston ,subset =train ,
                        mtry=6, importance =TRUE)
yhat.rf = predict(rf.boston ,newdata =Boston[-train ,])
mean((yhat.rf -boston.test)^2)
# Using the importance() function, we can view the importance of each
# importance() variable.
importance(rf.boston)
# Two measures of variable importance are reported. The former is based
# upon the mean decrease of accuracy in predictions on the out of bag samples
# when a given variable is excluded from the model. The latter is a measure
# of the total decrease in node impurity that results from splits over that
# variable, averaged over all trees
varImpPlot(rf.boston)

# boosting
# gbm package,
library(gbm)
# We run gbm() with the option distribution="gaussian" since this is a regression problem; if it were a binary
# classification problem, we would use distribution="bernoulli".
boost.boston =gbm(medv~.,data=Boston[train ,], distribution=
                      "gaussian",n.trees =5000 , interaction.depth =4)
summary(boost.boston)

# We can also produce partial dependence plots for these two variables. These plots
# partial dependence plot illustrate the marginal effect of the selected variables on the response after
# integrating out the other variables.
par(mfrow =c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")
# predict
yhat.boost=predict (boost.boston ,newdata =Boston[-train ,],
                    n.trees =5000)
mean((yhat.boost -boston.test)^2)
# If we want to, we can perform boosting with a different value of the shrinkage parameter ??
boost.boston =gbm(medv~.,data=Boston [train ,], distribution=
                      "gaussian",n.trees =5000 , interaction.depth =4, shrinkage =0.2,
                  verbose =F)
yhat.boost=predict(boost.boston ,newdata =Boston[-train ,],
                    n.trees =5000)
mean((yhat.boost -boston.test)^2)

# applied Q 10
# Remove the observations for whom the salary information is
# unknown, and then log-transform the salaries.
library(ISLR)
sum(is.na(Hitters$Salary))
Hitters = Hitters[-which(is.na(Hitters$Salary)), ]
sum(is.na(Hitters$Salary))
Hitters$Salary = log(Hitters$Salary)
# create training set N=200
train = 1:200
Hitters.train = Hitters[train, ]
Hitters.test = Hitters[-train, ]
# boosting; range of values of the shrinkage parameter lambda
set.seed(103)
pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
length.lambdas = length(lambdas)
train.errors = rep(NA, length.lambdas)
test.errors = rep(NA, length.lambdas)
for (i in 1:length.lambdas) {
    boost.hitters = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", 
                        n.trees = 1000, shrinkage = lambdas[i])
    train.pred = predict(boost.hitters, Hitters.train, n.trees = 1000)
    test.pred = predict(boost.hitters, Hitters.test, n.trees = 1000)
    train.errors[i] = mean((Hitters.train$Salary - train.pred)^2)
    test.errors[i] = mean((Hitters.test$Salary - test.pred)^2)
}
plot(lambdas, train.errors, type = "b", xlab = "Shrinkage", ylab = "Train MSE", 
     col = "blue", pch = 20)
plot(lambdas, test.errors, type = "b", xlab = "Shrinkage", ylab = "Test MSE", 
     col = "red", pch = 20)
lambdas[which.min(test.errors)]

lm.fit = lm(Salary ~ ., data = Hitters.train)
lm.pred = predict(lm.fit, Hitters.test)
mean((Hitters.test$Salary - lm.pred)^2)
library(glmnet)
set.seed(134)
x = model.matrix(Salary ~ ., data = Hitters.train)
y = Hitters.train$Salary
x.test = model.matrix(Salary ~ ., data = Hitters.test)
lasso.fit = glmnet(x, y, alpha = 1)
lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
mean((Hitters.test$Salary - lasso.pred)^2)
boost.best = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", 
                 n.trees = 1000, shrinkage = lambdas[which.min(test.errors)])
summary(boost.best)

# bagging
set.seed(21)
rf.hitters = randomForest(Salary ~ ., data = Hitters.train, ntree = 500, mtry = 19)
rf.pred = predict(rf.hitters, Hitters.test)
mean((Hitters.test$Salary - rf.pred)^2)

#Q11
library(ISLR)
train = 1:1000
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train = Caravan[train, ]
Caravan.test = Caravan[-train, ]

set.seed(342)
boost.caravan = gbm(Purchase ~ ., data = Caravan.train, n.trees = 1000, shrinkage = 0.01, 
                    distribution = "bernoulli")
summary(boost.caravan)

boost.prob = predict(boost.caravan, Caravan.test, n.trees = 1000, type = "response")
boost.pred = ifelse(boost.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, boost.pred)
34/(137 + 34)
