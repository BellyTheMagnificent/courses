library(caret); 
library(kernlab); 
data(spam);
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type ~.,data=training, method="glm")

## Show parameter of Train()
args(train.default)

## Show parameter of trainControl()
args(trainControl)

## Seeding
set.seed(1235)
modelFit2 <- train(type ~.,data=training, method="glm")
modelFit2
