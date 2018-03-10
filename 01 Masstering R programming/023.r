###PCA study
data(Boston,package="MASS")
dim(Boston)
pca_out=prcomp(Boston,scale=T)
pca_mtcars=prcomp(mtcars,scale=T) 
biplot(pca_mtcars)
############ kmens
library(cluster)
### get the pca
pcas=prcomp(iris[,-5],scale=T)
summary(pcas)
pcmod=predict(pcas)
k_obj=kmeans(pcmod,3) 
plot(pcmod[,1],pcmod[,2])
