library(ISLR); 
library(caret);
library(splines)
data(Wage);
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,];
testing <- Wage[-inTrain,];

#convert factor variables to indicator variables
table(training$jobclass)

#Convert factor variable to matrix/binary form
dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))

# find out the predictor which have very low variance
nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv
nsv <- nearZeroVar(training,saveMetrics=FALSE)

## Spline, creating curve.
bsBasis <- bs(training$age,df=3) 
head(bsBasis)

lm1 <- lm(wage ~ bsBasis,data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)

predict(bsBasis,age=testing$age)
