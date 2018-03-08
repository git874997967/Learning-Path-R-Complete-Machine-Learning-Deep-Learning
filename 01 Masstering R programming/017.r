####feature selection with Boruta package
library(caret)
library(Boruta)
data(GlaucomaM, package = 'TH.data')
dim(GlaucomaM)
set.seed(4321)
sam = sample(2,
             nrow(GlaucomaM),
             replace = T,
             prob = c(0.85, 0.15))
train = GlaucomaM[sam == 1,]
test = GlaucomaM[sam == 2,]
###get most siginificant vars
borutaMod = Boruta(Class ~ ., train, doTrace = 1)
borutaMod

###
boruta_signi = getSelectedAttributes(borutaMod, withTentative = T)
print(boruta_signi)
###do a rough fix on tentative variables
roughFixMod = TentativeRoughFix(borutaMod)
boruta_sigi2 = getSelectedAttributes(roughFixMod, withTentative = T)
boruta_sigi2
plot(roughFixMod)
###get the score and
attStats(roughFixMod)
imps_sort = imps[order(-imps$meanImp), ]
rownames(imps_sort)
imps_sort
#### impVar in caret
###
library(doMC)
registerDoMC(cores = 4)
x=train[,-63]
y=train[,63]
testX=test[,-63]

##number of featuers to be retained
subsets=c(1:5,10,15,20,25,35,45,55)
###remove highly correlated predictors
correls=findCorrelation(cor(x),cutoff=.9)
if(length(correls)!=0){
  x=x[,-correls]
}
###create folds
set.seed(100)
index=createFolds(y,k=10,returnTrain=T)

