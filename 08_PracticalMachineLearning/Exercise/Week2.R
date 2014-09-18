# Q1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Q2
library(AppliedPredictiveModeling)
library(Hmisc)
library(ggplot2)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
str(training)
Cement = cut2(training$Cement, g=4)
BlastFurnaceSlag= cut2(training$BlastFurnaceSlag, g=4)
Age= cut2(training$Age, g=4)
FlyAsh =  cut2(training$FlyAsh, g=4)
Water =  cut2(training$Water, g=4)
Superplasticizer =  cut2(training$Superplasticizer, g=4)
CoarseAggregate =  cut2(training$CoarseAggregate, g=4)
FineAggregate =  cut2(training$FineAggregate, g=4)
plot(training$CompressiveStrength)
plot(training$CompressiveStrength, col=Cement)
plot(training$CompressiveStrength, col=BlastFurnaceSlag)
plot(training$CompressiveStrength, col=Age)
plot(training$CompressiveStrength, col=FlyAsh)
plot(training$CompressiveStrength, col=Water)
plot(training$CompressiveStrength, col=Superplasticizer)
plot(training$CompressiveStrength, col=CoarseAggregate)
plot(training$CompressiveStrength, col=FineAggregate)

# Q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
summary(training$Superplasticizer)
qplot(Superplasticizer, data=training)
qplot(log(Superplasticizer), data=training)


#Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
str(training)
smallTraining = training[,c("IL_11","IL_13","IL_16",
                            "IL_17E","IL_1alpha","IL_3","IL_4","IL_5",
                            "IL_6","IL_6_Receptor","IL_7","IL_8",
                            "IP_10_Inducible_Protein_10")]
prComp <- prcomp(smallTraining)
plot(prComp$x[,1],prComp$x[,2])
preProc <- preProcess(training[,c("IL_11","IL_13","IL_16",
                                  "IL_17E","IL_1alpha","IL_3","IL_4","IL_5",
                                  "IL_6","IL_6_Receptor","IL_7","IL_8")],
                      data=training,method="pca",thresh=0.90)
str(training)
names(preProc)
## Number of component to meet threshhold 0.9
preProc$numComp
preProc$call
preProc$pcaComp
preProc$k
preProc$ica
# Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
str(adData)
modelFit <- train(diagnosis ~IL_11+IL_13+IL_16+
                        IL_17E+IL_1alpha+IL_3+IL_4+IL_5+
                        IL_6+IL_6_Receptor+IL_7+IL_8 ,method="glm", data=training)
confusionMatrix(testing$diagnosis,predict(modelFit,testing))

preProc <- preProcess(training[,c("IL_11","IL_13","IL_16",
                                  "IL_17E","IL_1alpha","IL_3","IL_4","IL_5",
                                  "IL_6","IL_6_Receptor","IL_7","IL_8")],
                      data=training,method="pca",thresh=0.8)
trainPC <- predict(preProc,training[,c("IL_11","IL_13","IL_16",
                                       "IL_17E","IL_1alpha","IL_3","IL_4","IL_5",
                                       "IL_6","IL_6_Receptor","IL_7","IL_8")])
names(trainPC)
modelFit.pca <- train(training$diagnosis ~ .,method="glm",data=trainPC)
testPC <- predict(preProc,testing[,c("IL_11","IL_13","IL_16",
                                      "IL_17E","IL_1alpha","IL_3","IL_4","IL_5",
                                      "IL_6","IL_6_Receptor","IL_7","IL_8")])
confusionMatrix(testing$diagnosis,predict(modelFit.pca,testPC))
