library(caret); 
library(kernlab); 
data(spam);
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE);
training <- spam[inTrain,]
testing <- spam[-inTrain,]

## Correlation
M <- abs(cor(training[,-58]))

## Finding column which highly correlate each other
diag(M) <- 0
which(M > 0.8,arr.ind=T)

# Found variable 32, and 34
names(spam)[c(34,32)]

plot(spam[,34],spam[,32])

# Rotate the plot
X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

# prcomp - Principal Component (PCA)
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

## Rotation
prComp$rotation

typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

## Building PCA using Caret
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

# Preprocessing with Caret
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)

testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

## Alternative
modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))

head(preProc)
