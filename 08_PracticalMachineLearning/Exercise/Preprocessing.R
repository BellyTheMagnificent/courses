library(caret);
library(kernlab);
library(RANN)
data(spam)
inTrain = createDataPartition(y=spam$type, 
                              p=0.75, list=FALSE)
training = spam[inTrain,]
testing = spam[-inTrain,]
hist(training$capitalAve, main = "",
     xlab="ave. capital run length")

mean(training$capitalAve)

## Standardizing
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(trainCapAveS)
sd(trainCapAveS)

## Standardizing - Test Set
testCapAve <- testing$capitalAve
## !! Use Training Set as input
testCapAveS <- (testCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(testCapAveS)
sd(testCapAveS)

## Standardizing - preProcess
# Column 58 is the outcome: Type
preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

## Standardizing - preProcess on Test Set
testCapAveS <- predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

## Standardizing - preProcess argument in train() 
modelFit <- train(type ~.,data=training,
                  preProcess=c("center","scale"),method="glm")

modelFit

## Standardizing - Box-Cox transforms
preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2));
hist(trainCapAveS); 
qqnorm(trainCapAveS)

## Standardizing - Imputing data (Impute missing data) 
set.seed(13343)
# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve - capAveTruth)

## Imputed values only
quantile((capAve - capAveTruth)[selectNA])

## Original
quantile((capAve - capAveTruth)[!selectNA])