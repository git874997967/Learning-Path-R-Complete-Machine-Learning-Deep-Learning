library(car)
library(corrplot)
library(mice)
data(Prestige)
head(Prestige)
summary(Prestige)
str(Prestige)
library(Hmisc)
describe(Prestige)
library(MASS)
data("Cars93")
summary(Cars93)
summary(Cars93$Price)
quantile(Cars93$Price, prob = 0.96)
###cor  between numeric vars
cor(Prestige$education, Prestige$income)
cor.test(Prestige$education, Prestige$income)
### annova  between numeric and caterical vars
boxplot(income ~ type, data = Prestige)
aovmod = aov(income ~ type, data = Prestige)
summary(aovmod)
###chi sq test   both caterical vars
Prestige$income_cat = dplyr::ntile(Prestige$income, 4)
table(Prestige$income_cat, Prestige$type)
chisq.test(x = Prestige$income_cat, y = Prestige$type)
cor.test(Prestige$income, Prestige$census)
corrplot(cor(Prestige[,-6]), method = "pie")
#### how to fix the outliers
myVar = Prestige$income
iqr = IQR(Prestige$income)
myVar[myVar > (3 * iqr)]
third_quartile = quantile(myVar, 0.75)
myVar[myVar > (1.5 * third_quartile)]
myVar[myVar > quantile(myVar, 0.99)]
com = mice(Prestige, method = 'rf')
com$predictorMatrix
com$imp$type
Prestige$type[is.na(Prestige$type)] = com$imp$type$`2`
anyNA(Prestige)

summary(Cars93)
com2 = mice(Cars93, method = 'rf', seed = 4321)
### use linear regression
sam = sample(2,
             nrow(Prestige),
             prob = c(0.85, 0.15),
             replace = T)
train = Prestige[sam == 1, ]
test = Prestige[sam == 2, ]
lmmod = lm(prestige ~ log(income) + education + type + census, data = train)
summary(lmmod)
lmmod2 = lm(prestige ~ ., data = train)
low = lm(prestige ~ 1, data = train)
full = lm(prestige ~ ., data = train)
lmmod3 = step(
  low,
  scope = list(lower = low, upper = full),
  direction = 'both',
  trace = 1,
  steps = 1000
)
summary(lmmod3)
lmpred = predict(lmmod, test)
mean((test$prestige - exp(lmpred)) ^ 2)
DMwR::regr.eval(test$prestige, exp(lmpred))
vif(lmmod2)
#####   Residuals and fittered and scale-location
## the points in the plot should be random  and red line shoud
####  flat
### the  normal QQ  should as linear as possible
par(mfrow = c(2, 2))
plot(lmmod)
hist(Prestige$income)
####with annova  to determin which var is not necessary  each should be the subset of next model
mod1 = lm(prestige ~ education, data = train)
mod2 = lm(prestige ~ education + income, data = train)
mod3 = lm(prestige ~ education + income + type, data = train)
mod4 = lm(prestige ~ education + income + type + census, data = train)
anova(mod1, mod2, mod3, mod4)
summary(lmmod3)
lmmod4 = update(lmmod3, . ~ . - census, data = train)
summary(lmmod4)
####we can delete the var with the highest gvif value  make the model more stable
vif(lmmod4)
####use k fold cross validation

glmod=glm(prestige~log(income)+education+type+census+type:income,data=Prestige)
summary(glmod)
cv.glm(Prestige,glmod,K=5)$delta
