# Q1 
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart.plot)

str(segmentationOriginal)
set.seed(125)
# splitting
# split = createDataPartition(y=segmentationOriginal$Case, p=0.70, list=FALSE)
training = subset(segmentationOriginal,Case =="Train")
testing = subset(segmentationOriginal, Case == "Test")
# build regression tree model using caret package
CART = train(Class ~., method = "rpart", data=training)
print(CART$finalModel)
#plot(CART$finalModel, uniform=TRUE, main="Classification Tree")
#text(CART$finalModel, use.n=TRUE, all=TRUE, cex=.9)
## plot tree using rpart.plot library
prp(CART$finalModel)

# Q3
library(pgmm)
data(olive)
olive = olive[,-1]
str(olive)
summary(olive)
newdata = as.data.frame(t(colMeans(olive)))
olive.train = train(Area ~., method="rpart", data=olive)
olive.predict = predict(olive.train, newdata=newdata)
predict(olive.train,newdata=newdata)
prp(olive.train$finalModel)

# Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
str(trainSA)
set.seed(13234)

## Train Model
modelFit = train(chd ~ age+alcohol+obesity+tobacco+typea+ldl, method="glm", data =trainSA, family="binomial")
## build quiz function
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
modelFit.predict = predict(modelFit, newdata=testSA)
modelFit.predict.train = predict(modelFit, newdata=trainSA)
missClass(testSA$chd, modelFit.predict)
missClass(trainSA$chd, modelFit.predict.train)


# Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
str(vowel.train)
vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)
str(vowel.train)
set.seed(33833)
rf.modelFit = train(y ~. , method="rf", data=vowel.train, importance=TRUE)
# GINI importance
importance(rf.modelFit$finalModel, type = 1)
varImp(rf.modelFit)
importance(rf.modelFit$finalModel, type=2)
rf.modelFit$finalModel$importance
