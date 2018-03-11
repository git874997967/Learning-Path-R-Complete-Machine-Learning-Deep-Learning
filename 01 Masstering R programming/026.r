####hierarchical clustering
dta = read.csv(file.choose())
colnames(dta) = c(
  "area",
  "perimeter",
  "compactness",
  "length_of_kernel",
  "width_of_kernel",
  "asymmetry_coefficient",
  "length_of_kernel_groove",
  "type"
)
library(cluster)


dist_mat = dist(dta[, -8])
h = hclust(dist_mat, method = 'ward.D')

plot(h)
# plot.new()
rect.hclust(h, k = 3, border = 2:4)
###cutree function will group tree in diff groups by
###obj is the result of hclust tree
###k is the group numbers
###h is the high that should be cut
pred = cutree(h, k = 3)
pred
table(pred, dta$type)
####
set.seed(4321)
dist_iris = dist(iris[, -5])
h_iris = hclust(dist_iris, method = 'ward.D')
pred_iris = cutree(h_iris, k = 3)
plot(h_iris)
pred_iris
table(pred_iris, iris$Species)

#######AP cluster
library(apcluster)
head(dta)
#### param r is like the knn    which kingd of distance we should use
neg_sim_mat = negDistMat(dta[, -8], r = 2)
dim(neg_sim_mat)
lus = apcluster(neg_sim_mat)
lus
cl = lus@clusters   #get the clusters
xmplars = lus@exemplars #### exemplars
####tidy the clusters
tidy_clus=function(cl){
  names(cl)=paste0('cl',1:length(cl))
}
####function to get the observation and cluster number in a dataframe
pca_iris_mod=princomp(iris[,-5])
screeplot(pca_iris_mod,type='lines')
pca_iris=predict(pca_iris_mod)
head(pca_iris)
iris_clus=apcluster(negDistMat(r=2),pca_iris[,1:2])
iris_clus@clusters
iris_clus@exemplars
