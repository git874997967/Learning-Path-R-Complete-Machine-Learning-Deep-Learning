####svm
library(caret)
library(doParallel)
data("segmentationData")
set.seed(4321)
sam = sample(2,
             nrow(segmentationData),
             replace = T,
             prob = c(0.85, 0.15))
train = segmentationData[sam == 1,-c(1:2)]
test = segmentationData[sam == 2,-c(1:2)]
table(segmentationData$Class)
ctrl = trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats = 5,
  classProbs = T,
  sampling = 'down',
  summaryFunction = twoClassSummary,
  verboseIter = T
)
registerDoParallel(6)		                        # Registrer a parallel backend for train
getDoParWorkers()
##svmlinear
grid = expand.grid(C = c(0.25, 0.5, 0.75, 1, 1, 1.25, 1.5))
svmLin_mod = train(
  Class ~ .,
  train,
  trControl = ctrl,
  method = 'svmLinear',
  preProc = c('center', 'scale'),
  tuneGrid = grid,
  metric = 'ROC'
)
svmLin_mod
### svmRadial
grid2 = expand.grid(C = c(0.25, 0.5, 0.75, 1, 1, 1.25, 1.5),
                    sigma = c(.01, .015, .02))
svmRad_mod = train(
  Class ~ .,
  train,
  trControl = ctrl,
  method = 'svmRadial',
  preProc = c('center', 'scale'),
  tuneGrid = grid2,
  metric = 'ROC'
)
####svm polynmial kernal
grid3 = expand.grid(
  C = c(0.25, 0.5, 0.75, 1, 1.25, 1.5),
  degree = c(1, 2, 3),
  scale = c(.001, .01, .1)
)

svmPoly_mod = train(
  Class ~ .,
  train,
  trControl = ctrl,
  method = 'svmPoly',
  preProc = c('center', 'scale'),
  tuneGrid = grid3,
  metric = 'ROC',
  tuneLength = 3###
)

###  resample used to compare diff kernels
comparsions = resamples(list(
  Poly = svmPoly_mod,
  radial = svmRad_mod,
  Linear = svmLin_mod
))
summary(comparsions)
 
bwplot(comparsions, metric = "ROC")

confusionMatrix(data=svmRad_mod,test$Class)
xyplot(comparsions, what = "BlandAltman")
summary(diff(comparsions))

###predict use the linear
linear_Predict = predict(svmLin_mod, test)
confusionMatrix(linear_Predict, test$Class)
radioal_Predict=predict(svmRad_mod,test)
confusionMatrix(radioal_Predict,test$Class)

###
# NOT RUN {
cat(sprintf('%s backend is registered\n',
            if(getDoParRegistered()) 'A' else 'No'))
sprintf('Running with %d worker(s)\n', getDoParWorkers())
(name <- getDoParName())
(ver <- getDoParVersion())
if (getDoParRegistered())
  cat(sprintf('Currently using %s [%s]\n', name, ver))
# }
par(mfrow=c(3,1))
plot(svmLin_mod)
plot(svmRad_mod)
plot(svmPoly_mod)