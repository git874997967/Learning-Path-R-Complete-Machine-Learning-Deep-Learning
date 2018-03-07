library(caret)
data(BreastCancer, package = "mlbench")
bc = na.omit(BreastCancer)
str(bc)
bc = bc[,-1]
###convert the factor to numeric
for (i in 1:9) {
  bc[, i] = as.numeric(as.character(bc[, i]))
}
bc$Class = as.factor(ifelse(bc$Class == 'malignant', 1, 0))
'%ni%' = Negate('%in%')
###train and test
sam = sample(2, nrow(bc), prob = c(0.85, 0.15), replace = T)
train = bc[sam == 1,]
test = bc[sam == 2,]
###downsampling
down_train = downSample(x = train[, colnames(train) %ni% 'Class'], y = train$Class)
####upsampling
up_train = upSample(x = train[, colnames(train) %ni% 'Class'], y = train$Class)
table(down_train$Class)
head(train)

###hybrid

###build the model
logitmod = glm(Class ~ Cl.thickness + Cell.size + Cell.shape,
               family = binomial,
               data = down_train)
summary(logitmod)
plot(logitmod)
### use binomial   regression use type response
down_pred = predict(logitmod, test, type = 'response')
y_pred=as.factor(ifelse(down_pred>0.5,1,0))
y_act=test$Class
###acuracy
mean(y_pred==y_act)
###confussion  matrix 
caret::confusionMatrix(y_pred,y_act,positive="1")
InformationValue::plotROC(y_act,down_pred)
InformationValue::AUROC(y_act,down_pred)










 