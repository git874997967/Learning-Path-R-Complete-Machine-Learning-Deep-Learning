###use caret
library(caret)
### normoalize the range from 0-1
preprocess = preProcess(iris[, 1:4], method = 'center')
preprocess
### apply to the transform
normalized = predict(preprocess, iris[, 1:4])
iris_n = cbind(normalized, Species = iris$Species)
####summarize
summary(iris_n)
sam = sample(2, replace = T, prob = c(0.85, 0.15), nrow(iris))
train = iris[sam == 1, ]
test = iris[sam == 2, ]
fitControl = trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats = 5,
  summaryFunction = multiClassSummary,
  classProbs = T,
  verboseIter = F
)
grid = expand.grid(k = c(5, 7, 11, 13, 17, 19, 23, 25))
set.seed(4321)
knn_mod = train(
  Species ~ .,
  data = train,
  preProcess = c('range'),
  trControl = fitControl,
  method = 'knn',
  tuneGrid = grid,
  metric = c("Kappa"),
  tuneLength = 20
)
knn_mod$finalModel
knn_mod$bestTune
data(Vehicle, package = 'mlbench')
dim(Vehicle)
sam = sample(2,
             nrow(Vehicle),
             replace = T,
             prob = c(0.85, 0.15))
train = Vehicle[sam == 1, ]
test = Vehicle[sam == 2, ]
install.packages("doMC", repos = "http://R-Forge.R-project.org")
library(doMC)
tc = trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats = 5,
  classProbs = T,
  summaryFunction = multiClassSummary,
  verboseIter = T,
  search = 'random'
)
registerDoMC(cores = 8)
fit2=train(
  Class~.,
  train,
  trControl=tc,
  method='C5.0',
  metric='Kappa',
  tuneLength=5
)

fit$bestTune
fit2$bestTune
