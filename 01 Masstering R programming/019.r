###study ridge and lasso regression
library(caret)
library(glmnet)
library(lasso2)
###???
options(scipen = 999)
data(Prostate)
set.seed(4321)
trainRows = createDataPartition(Prostate$lpsa, p = 0.85, list = F)
train = Prostate[trainRows, ]
test = Prostate[-trainRows, ]
"%ni%" = Negate('%in%')
train_x = as.matrix(train[, colnames(train) %ni% 'lpsa'])
train_y = as.matrix(train[, 'lpsa'])
test_x = as.matrix(test[, colnames(test) %ni% 'lpsa'])
test_y = as.matrix(test[, 'lpsa'])
### step build the grid
grid = 10 ^ seq(10, -2, length = 100)
### buid the model
ridgeMod = glmnet(train_x,
                  train_y,
                  alpha = 0,
                  lambda = grid,
                  thresh = 1e-12)
summary(ridgeMod)
### FIND THE BEST DEV VALUE USE CV.GLMNET FUNCTIOn
###step3
set.seed(4321)
cv.out = cv.glmnet(train_x, train_y, alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
### step4   predict with the best value

pred = predict(ridgeMod, s = bestlam, newx = test_x)
DMwR::regr.eval(test_y, pred)
### plot
coefs_ridge = predict(ridgeMod, type = 'coefficients', s = bestlam)
plot(ridgeMod, xvar = "lambda")


#####build with lasso  alpha=1
lassoMod = glmnet(train_x,
                  train_y,
                  alpha = 1,
                  lambda = grid,
                  thresh = 1e-12)
summary(lassoMod)
cv.out1 = cv.glmnet(train_x, train_y, alpha = 1)
plot(cv.out1)
bestlam1 = cv.out1$lambda.min
pred1 = predict(lassoMod, s = bestlam1, newx = test_x)
DMwR::regr.eval(test_y, pred1)
plot(lassoMod, xvar = 'lambda')
####  combine models change alphas
alphas = seq(0, 1, by = .01)
foldid = sample(1:10, size = length(train_y), replace = T)
grid = 10 ^ seq(10, -2, length = 100)
mapes = numeric(length(alphas))
for (a in alphas) {
  bestlam1 = cv.glmnet(
    train_x,
    train_y,
    alpha = a,
    lambda = grid,
    foldid = foldid
  )$lambda.min
  enetMod = glmnet(train_x, train_y, alpha = a, lambda = bestlam1)
  pred = predict(enetMod, newx = test_y, s = bestlam1)
  mapes[i] = DMwR::regr.eval(test_y, pred)[4]
  i = i + 1
}
out = cbind(alphas, mapes)
out
