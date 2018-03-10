####xgboost
library(xgboost)
library(caret)
library(Matrix)
library(DiagrammeR)
options(scripen = 999)
'%ni%' = Negate('%in%')
### prepare the datasets
data("Prostate", package = 'lasso2')
set.seed(4321)
trainRows = createDataPartition(Prostate$lpsa, p = .85, list = F)
train = Prostate[trainRows,]
test = Prostate[-trainRows,]
###create the dummy model  for xgboost

trainData_xg = xgb.DMatrix(data.matrix(train[, colnames(train) %ni% 'lpsa']),
                           label = as.numeric(train$lpsa))
testData_xg = xgb.DMatrix(data.matrix(test[, colnames(test) %ni% 'lpsa']))
watchlist = list(train = trainData_xg, test = testData_xg)
head(watchlist$train)


param = list("objective" = 'reg:linear',
             "eval_matric" = "rmse")
cv.nround = 50
cv.nfold = 10
cvMod = xgb.cv(
  param = param,
  data = trainData_xg,
  nfold = cv.nfold,
  nrounds = cv.nround
)
### get the bestfold
nround = which(cvMod$evaluation_log$test_rmse_mean == min(cvMod$evaluation_log$test_rmse_mean))
xgMod = xgb.train(
  param = param,
  data = trainData_xg,
  nrounds = nround,
  booster = 'gblinear'
)
xgMod_pred = predict(xgMod, testData_xg)

DMwR::regr.eval(xgMod_pred, test$lpsa)

###   pack the  training control parapmeters
library(doParallel)
registerDoParallel(cores = 6)
getDoParWorkers()
xgb_trcontrol = trainControl(
  method = 'cv',
  number = 5,
  verboseIter = T,
  returnData = F,
  returnResamp = 'all',
  allowParallel = T
)

### in the caret  package the xgblinear and xgbtree already installed
## and the usage is much easer and much familar with that
modelLookup('xgbLinear')
set.seed(4321)
xgb_train = train(
  x = as.matrix(train[, colnames(train) %ni% 'lpsa']),
  y = train$lpsa,
  trControl = xgb_trcontrol,
  tuneLength = 3,
  method = 'xgbLinear',
  metric = 'RMSE'
)
###
xgb_train = train(
  x = as.matrix(train[, colnames(train) %ni% 'lpsa']),
  y = train$lpsa,
  trControl = trainControl(method = 'none'),
  tuneGrid = expand.grid(
    nrounds = 50,
    lambda = 0.1,
    alpha = 0.1,
    eta = 0.3
  ),
  method = 'xgbLinear',
  metric = 'RMSE'
)
xgb_pred = predict(xgb_train, test)
DMwR::regr.eval(xgb_pred, test$lpsa)
#############exercise
data(Glass, package = 'mlbench')

str(Glass)
####
library(dplyr)
###split the train&test
Glass_train = Glass[Glassrow,]
Glass_test = Glass[-Glassrow,]
target = Glass_train$Type %>% as.integer
target = target - 1
table(Glass_test$Type)

###convert the matrix
train_matrix = Glass_train[,-c(1, ncol(Glass_train))] %>% data.matrix
test_matrix = Glass_test[,-c(1, ncol(Glass_test))] %>% data.matrix

#level(target)
nclass = 6
param = list(
  "objective" = 'multi:softprob',
  'eval_metric' = 'merror',
  'num_class' = nclass
)
cv.nround = 500
cv.nfold = 10
bst.cv = xgb.cv(
  param,
  train_matrix,
  label = target,
  nfold = cv.nfold,
  nrounds = cv.nround
)
which(
  bst.cv$evaluation_log$test_merror_mean == min(bst.cv$evaluation_log$test_merror_mean)
)
nround = 11
bst = xgboost(
  data = train_matrix,
  param = param,
  label = target,
  nround = nround
)
Glass_xgb_pred = predict(bst, test_matrix)



######

Glass_train_xgb_DM = xgb.DMatrix(data.matrix(Glass_train[, colnames(Glass_train) %ni%
                                                           'Type']), label =
                                   target)
Glass_test_xgb_DM = xgb.DMatrix(data.matrix(Glass_test[, colnames(Glass_test) %ni%
                                                         
                                                         
                                                         ### here we have the watchlist  what that mean?
                                                         'Type']))
watchlist = list(train = Glass_train_xgb_DM, test = Glass_test_xgb_DM)

Glass_param = list(
  "objective" = "multi:softmax",
  "eval_metric" = 'mlogloss',
  'num_class' = nclass
)
cv.nround = 500
cv.nfold = 10
cvMod = xgb.cv(
  param = Glass_param,
  data = Glass_train_xgb_DM,
  nfold = cv.nfold,
  nrounds = cv.nround,
  watchlist = watchlist
)
cv.bst_nround = which(
  cvMod$evaluation_log$test_mlogloss_mean == min(cvMod$evaluation_log$test_mlogloss_mean)
)
cvMod_final = xgboost(param = Glass_param,
                      data = Glass_train_xgb_DM,
                      nround = cv.bst_nround)
length(Glass_test_xgb_DM)


###feature importance matrix
names = colnames(Glass_train)[colnames(Glass_train) %ni% 'Type']
featureImp = xgb.importance(names, model = cvMod_final)
xgb.plot.importance(featureImp)
featureImp
xgb.plot.tree(feature_names = names,
              model = cvMod_final,
              n_first_tree = 2)


Glass_pred = predict(cvMod_final, Glass_test_xgb_DM)
tab = table(Glass_pred, Glass_test$Type)
caret::confusionMatrix(table(Glass_pred, Glass_test$Type))



####  summary  the key to use  classification in xgboost is to make types into : n
##otherwise it will not works