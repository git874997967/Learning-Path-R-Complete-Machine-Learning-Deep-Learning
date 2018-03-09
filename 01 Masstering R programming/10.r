library(caret)
data(BreastCancer, package = "mlbench")
bc = na.omit(BreastCancer)
str(bc)
bc = bc[, -1]
###convert the factor to numeric
for (i in 1:9) {
  bc[, i] = as.numeric(as.character(bc[, i]))
}
bc$Class = as.factor(ifelse(bc$Class == 'malignant', 1, 0))
'%ni%' = Negate('%in%')
###train and test
sam = sample(2, nrow(bc), prob = c(0.85, 0.15), replace = T)
train = bc[sam == 1, ]
test = bc[sam == 2, ]
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
y_pred = as.factor(ifelse(down_pred > 0.5, 1, 0))
y_act = test$Class
###acuracy
mean(y_pred == y_act)
###confussion  matrix
caret::confusionMatrix(y_pred, y_act, positive = "1")
InformationValue::plotROC(y_act, down_pred)
InformationValue::AUROC(y_act, down_pred)

######Naive Bayes  study
data(Vehicle, package = 'mlbench')
head(Vehicle)
str(Vehicle)
summary(Vehicle)
library(klaR)
samp = sample(2,
              nrow(Vehicle),
              prob = c(0.85, 0.15),
              replace = T)
train = Vehicle[samp == 1, ]
test = Vehicle[samp == 2, ]

nb_mod = NaiveBayes(Class ~ ., train)
nb_pred = predict(nb_mod, test)
mean(test$Class != nb_pred)

caret::featurePlot(Vehicle[, -19], Vehicle[, 19], plot = 'box')

tab = table(nb_pred$class, test$Class)
caret::confusionMatrix(tab)
InformationValue::plotROC(nb_mod, test$Class)
plot(nb_mod)
data("iris")
sam = sample(2, nrow(iris), replace = T, prob = c(0.85, 0.15))
train = iris[sam == 1, ]
test = iris[sam == 2, ]
iris_mod = NaiveBayes(Species ~ ., train)
iris_pred = predict(iris_mod, test)
mean(iris_pred$class != test$Species)
iris_tab = table(iris_pred$class, test$Species)
caret::confusionMatrix(iris_tab)
# kappa > 0.1，说明模型凑合，马马虎虎
# kappa > 0.4，说明模型还行
# kappa > 0.8，说明模型挺好的
###tree mod
library(partykit)
ctmod=ctree(Species~.,train)
plot(ctmod)
ct_pred=predict(ctmod,test)
iris_tab_ctree=table(ct_pred,test$Species)
caret::confusionMatrix(iris_tab_ctree)
rpmod=rpart(Species~.,train,control=rpart.control(minsplit=5,cp=0.0001,maxdepth=4))
rpmod$cptable
plot(rpmod)
text(rpmod)
typeof(rp_pred)
rp_pred=predict(rpmod,test)
iris_tab_rp=table(rp_pred,test$Species)
caret::confusionMatrix(table(rp_pred,test$Species))
