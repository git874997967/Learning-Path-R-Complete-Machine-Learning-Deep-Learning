####recommand engines
library(recommenderlab)
ratingDf = read.csv("https://goo.gl/HyQcAX")
###read the data
head(ratingDf)
###convert into spase matrix
ratingMt = as.matrix(ratingDf)
View(head(ratingMt))
###convert the matrix into certain format
ratings = as(ratingMt, 'realRatingMatrix')

###get the user similarity
###different method could be used
###which means among users
usr_sim = recommenderlab::similarity(ratings[1:10, ], method = 'cosine',
                                     which = 'users')
###take care there are NA values
usr_sim[is.na(usr_sim)] = 0
head(usr_sim)
####round the value
usr_sim = round(usr_sim, 2)
##plot it
plot(hclust(usr_sim))
###find similarity base on user preference item_sim
item_sim = recommenderlab::similarity(ratings[, 1:15], method = 'cosine', which =
                                        'items')
item_sim[is.na(item_sim)] = 0
item_sim = round(item_sim, 2)
plot(hclust(item_sim))
### all methods could be used
# recommenderRegistry$get_entries()
# recommender_models = recommenderRegistry$get_entries(dataType = "realRatingMatrix")
# recommender_models
# ###default functions
# names(recommender_models)
# recommender_models$UBCF_realRatingMatrix
###split   train and test dataset
train_rows=sample(2,nrow(ratings),prob=c(0.85,0.15),replace = T)
train=ratings[train_rows==1,]
test=ratings[train_rows==2,]
###get the model
rec_model=Recommender(train,method='UBCF')
getModel(rec_model)
###test   recommond 15 movies for users in testdataset
  n_reco=15
recommendations=predict(object=rec_model,newdata=test,n=n_reco)
# recommendations 
# recommendations@items
# recommendations@ratings
# recommendations@itemLabels
reco_out=as(recommendations,'list')
reco_out
###smaller dataset   top3
top3=bestN(recommendations,3)
top3=as(top3,'list')
top3[[106]]
reco_out[[106]][1:3]
length(reco_out)
reco_out
###use popular method
rec_model_popular=Recommender(train,method='POPULAR')

n_reco=5
###almost the same with normal predict  but add numbers for each user
recommendations_pop=predict(rec_model_popular,test,n_reco)
as(recommendations_pop,'list')
reco_Pop_top3=bestN(recommendations_pop,3)
###do not forget to transform into list format
reco_Pop_top3=as(reco_Pop_top3,'list')
reco_Pop_top3[106]



