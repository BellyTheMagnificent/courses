library(caret); 
library(kernlab); 
data(spam);

## Create splitting index using createDataPartition (For binary / nominal data type)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

## K-Fold
set.seed(32323)
## splitting to 10 folds and return training set
folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain=TRUE)
sapply(folds,length)
## grab 1st fold
folds[[1]][1:10]
# Return test
set.seed(32323)
folds <- createFolds(y=spam$type,k=10, list=TRUE,returnTrain=FALSE)
sapply(folds,length)

## Resampling (bootstraping)
set.seed(32323)
folds <- createResample(y=spam$type,times=10,list=TRUE)
sapply(folds,length)

## Time series 
set.seed(32323)
tme <- 1:1000
## Use 1 to 20 records to predict 21st to 30th records
folds <- createTimeSlices(y=tme,initialWindow=20,horizon=10)
names(folds)
folds$train[[1]]
folds$train[[2]]
folds$train[[3]]
folds$test[[1]]
