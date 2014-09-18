library(caret);
data(faithful);
set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,  p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; 
testFaith <- faithful[-inTrain,]
head(trainFaith)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")

## Fitting linear 
lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

## Draw line into plot
lines(trainFaith$waiting,lm1$fitted,lwd=3)

# Predict a new value (Manual)
coef(lm1)[1] + coef(lm1)[2]*80

# Using Predict Library
newdata <- data.frame(waiting=80)
predict(lm1,newdata)

# Comparing prediction and test set using plot
par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
## Test set has shifted a bit
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

## Calculatin error (RMSE)
# Calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))

# Calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))

# drawing interval line (interpretation)
pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)

## Using Caret
modFit <- train(eruptions ~ waiting,data=trainFaith,method="lm")
summary(modFit$finalModel)