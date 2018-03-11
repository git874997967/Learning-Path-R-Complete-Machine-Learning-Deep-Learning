###PCA study
data(Boston, package = "MASS")
dim(Boston)
pca_out = prcomp(Boston, scale = T)
pca_mtcars = prcomp(mtcars, scale = T)
biplot(pca_mtcars)
############ kmens
library(cluster)
### get the pca
pcas = prcomp(iris[,-5], scale = T)
summary(pcas)
pcmod = predict(pcas)
k_obj = kmeans(pcmod, 3)
plot(pcmod[, 1], pcmod[, 2])
####   kmeans and clusters are more related with the unsupervsied study
###whcich means we do not have the train label instead of exploring them by
#ourselves
###Hopkins   statis index
library(clustertend)
### because we will add points randomly  which means we should set the seed
###to get the same result

set.seed(4321)
hopkins(iris[, -5], n = nrow(iris) - 1)
### the result is around 17%  so the distribution are not random
###vfisulize the clustering tendency
library(seriation)
### this is the random pattern
rand_mat = matrix(runif(1000), nrow = 100)
dissplot(dist(rand_mat), main = 'clustering Tendency dissplot')
###for iris
df_dist = dist(iris[,-5])
dissplot(dist(df_dist), main = 'Clustering tendency dissplot')
table(iris$Species)
library(cluster)
data(ruspini, package = 'cluster')
x = ruspini
avg_sil_wid = numeric(NROW(x) / 2)
for (nclus in 2:(nrow(x) / 2)) {
  set.seed(4321)
  kout = kmeans(x, nclus)  ### run kmeans
  ss = silhouette(kout$cluster, dist(x))###silouette plot
  avg_sil_wid[nclus] = mean(ss[, 3])
}
opt_clusters = which.max(avg_sil_wid) ###optclusters size
# opt_clusters
###plot it
# plot(1:(NROW(x)/2),avg_sil_wid,type='b',pch=19,fram=T)
# points(x=opt_clusters,y=max(avg_sil_wid),col='red',pch=21,cex=3)
# abline(v=which.max(avg_sil_wid),lty=2)
plot(1:(NROW(x) / 2), avg_sil_wid)
points(opt_clusters,
       max(avg_sil_wid),
       col = 'red',
       cex = 3)
abline(v = which.max(avg_sil_wid))
######
set.seed(4321)
kout = kmeans(x, opt_clusters)
kout$cluster
plot(x, col = kout$cluster)





######## exercise
hopkins(x, n = nrow(x) - 1)
###28%  result shows that they are not random
dissplot(dist(x))
#####
x = faithful
avg_sil_wid = numeric(NROW(x) / 4)
for (nclus in 2:(nrow(x) / 4)) {
  set.seed(4321)
  kout = kmeans(x, nclus)
  ss = silhouette(kout$cluster, dist(x))
  avg_sil_wid[nclus] = mean(ss[, 3])
}

opt_clusters = which.max(avg_sil_wid)
plot(1:(NROW(x) / 4), avg_sil_wid)
points(opt_clusters,
       max(avg_sil_wid),
       col = 'red',
       cex = 15)
abline(v = which.max(avg_sil_wid))

plot(x, col = kout$cluster)

##### cluster problems are the shortage and drawback


hopkins(Boston, n = nrow(Boston) - 1)
dissplot(dist(Boston))
## how to deside the divide number 4 ???
avg_sil_wid = numeric(NROW(Boston) / 4)
for (nclus in 2:(nrow(Boston) / 4))
{
  set.seed(4321)
  kout = kmeans(Boston, nclus)
  ss = silhouette(kout$cluster, dist(Boston))
  avg_sil_wid[nclus] = mean(ss[, 3])
}
###shows how many clusters we will get
opt_clusters = which.max(avg_sil_wid)
plot(1:(NROW(Boston) / 4), avg_sil_wid)
points(opt_clusters,
       max(avg_sil_wid),
       col = 'red',
       cex = 21)
abline(v = which.max(avg_sil_wid))

plot(Boston, col = kout$cluster)
head(Boston)


head(faithful)
