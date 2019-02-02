# remove all objects at once
rm(list=ls())

# load data
states =row.names(USArrests)
states
names(USArrests)

# rows is 1 , columns is 2
apply(USArrests , 2, mean)
apply(USArrests , 2, var)

# PCA
pr.out =prcomp(USArrests , scale =TRUE)
names(pr.out)
# The center and scale components correspond to the means and standard
# deviations of the variables that were used for scaling prior to implementing PCA.
pr.out$center
pr.out$scale
# The rotation matrix provides the principal component loadings; each column
# of pr.out$rotation contains the corresponding principal component loading vector
pr.out$rotation

# Rather the 50 ×4 matrix x has as its columns the principal component score vectors.
dim(pr.out$x)

# plot
# The scale=0 argument to biplot() ensures that the arrows are scaled to biplot()
# represent the loadings; other values for scale give slightly different biplots with different interpretations.
biplot(pr.out , scale =0)

# The prcomp() function also outputs the standard deviation of each principal component.
pr.out$sdev
# variance explained
pr.var =pr.out$sdev ^2
pr.var
# proportion explained
pve=pr.var/sum(pr.var)
pve

plot(pve , xlab="Principal Component", ylab="Proportion of
Variance Explained", ylim=c(0,1) ,type="b")
plot(cumsum (pve), xlab="Principal Component", ylab ="Cumulative Proportion of Variance Explained", ylim=c(0,1) ,
     type="b")

# Clustering
# kmeans
set.seed(2)
x=matrix(rnorm (50*2) , ncol =2)
x[1:25 ,1]=x[1:25 ,1]+3
x[1:25 ,2]=x[1:25 ,2] -4
km.out =kmeans(x,2, nstart =20)
km.out$cluster
plot(x, col =(km.out$cluster +1) , main="K-Means Clustering
Results with K=2", xlab ="", ylab="", pch =20, cex =2)

set.seed(4)
km.out =kmeans(x,3, nstart =20)
km.out
# To run the kmeans() function in R with multiple initial cluster assignments,
# we use the nstart argument. If a value of nstart greater than one
# is used, then K-means clustering will be performed using multiple random
# assignments in Step 1 of Algorithm 10.1, and the kmeans() function will report only the best results.
set.seed(3)
km.out =kmeans(x,3, nstart =1)
# Note that km.out$tot.withinss is the total within-cluster sum of squares,
# which we seek to minimize by performing K-means clustering
km.out$tot.withinss
km.out =kmeans(x,3, nstart =20)
km.out$tot.withinss
# We strongly recommend always running K-means clustering with a large
# value of nstart, such as 20 or 50, since otherwise an undesirable local optimum may be obtained.

# hierachical clustering
hc.complete =hclust(dist(x), method ="complete")
plot(hc.complete ,main ="Complete Linkage", xlab="", sub ="",
     cex =.9)
# To determine the cluster labels for each observation associated with a
# given cut of the dendrogram, we can use the cutree() function:
cutree (hc.complete , 2)

# To scale the variables before performing hierarchical clustering of the
#observations, we use the scale() function:
xsc=scale(x)
plot(hclust (dist(xsc), method ="complete"), main ="Hierarchical
Clustering with Scaled Features")

# Correlation-based distance can be computed using the as.dist()
x=matrix(rnorm (30*3) , ncol =3)
dd=as.dist(1- cor(t(x)))
plot(hclust (dd, method ="complete"), main=" Complete Linkage
with Correlation -Based Distance ", xlab="", sub ="")

# data example
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

# PCA
pr.out =prcomp(nci.data ,scale=TRUE)
# function assigns color to each element
Cols=function (vec){
     cols=rainbow(length (unique (vec)))
     return (cols[as.numeric (as.factor(vec))])
}
par(mfrow =c(1,2))
plot(pr.out$x[,1:2], col =Cols(nci.labs), pch =19,
     xlab ="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3) ], col =Cols(nci.labs), pch =19,
     xlab ="Z1",ylab="Z3")
# porportion of variance explained (PVE)
summary(pr.out)
# Note that the height of each bar in the bar plot is given by squaring the
# corresponding element of pr.out$sdev.
plot(pr.out)

pve =100* pr.out$sdev ^2/ sum(pr.out$sdev ^2)
plot(pve , type ="o", ylab="PVE ", xlab=" Principal Component ",
     col =" blue")
plot(cumsum (pve ), type="o", ylab =" Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")
# Note that the elements of pve can also be computed directly from the summary,
# summary(pr.out)$importance[2,], and the elements of cumsum(pve)
# are given by summary(pr.out)$importance[3,].)
