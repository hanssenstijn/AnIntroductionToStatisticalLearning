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
