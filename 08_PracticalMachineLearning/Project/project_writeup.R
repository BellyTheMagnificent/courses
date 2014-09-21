library(caret)
library(ggplot2)
library(rpart.plot)
library(Hmisc)
library(corrgram)
set.seed(8586141)

training_file = '/Users/JoseCLee/Documents/GIT/courses/08_PracticalMachineLearning/Project/data/pml-training.csv';
testing_pfile = '/Users/JoseCLee/Documents/GIT/courses/08_PracticalMachineLearning/Project/data/pml-testing.csv';
raw_training = read.csv(training_file, header=TRUE, na.strings = c(" ","","#DIV/0!"), );
private_testing = read.csv(testing_pfile, header=TRUE, na.strings = c(" ","","#DIV/0!"));

## temporary assign classes to private testing data for combine 2 dataset
private_testing$classe = NA 

## Assign problem id to train data set for combine and split indicator
raw_training$problem_id = NA

## combine data for pre-processing
combine = rbind(raw_training, private_testing)

## take out dataset machine data 
combine = subset(combine, select = -c(X, user_name, raw_timestamp_part_1,raw_timestamp_part_2,
                                      cvtd_timestamp,cvtd_timestamp, new_window, num_window))
summary(combine)
## Convert all predictors to numeric
## variable for list of names
predictors = vector()
for(i in names(combine[,1:152]))
{        
        combine[,i] = as.numeric(combine[,i])
        if (sum(is.na(combine[,i]))/length(combine[,i]) < 0.5)
        {                
                predictors = rbind(predictors, i)
        }
}

combine = subset(combine, select = c(predictors, "classe", "problem_id"))

## Splitting pre-process data to train and private set
train_data = subset(combine, is.na(problem_id)==TRUE)
train_data = subset(train_data, select = -c(problem_id))
test_data = subset(combine, problem_id %in% 1:20)
rownames(test_data) = 1:20
test_data = subset(train_data, select = -c(classe))

split = createDataPartition(train_data$classe, list=FALSE, p = 0.7)
training = train_data[split,]
testing = train_data[-split,]

summary(testing$classe)

## fitting with rpart, accuracy = 0.5456
fit.rpart = train(classe ~ ., data = training, method = "rpart")
print(fit.rpart$finalModel)
prp(fit.rpart$finalModel)
predict.rpart = predict(fit.rpart, newdata = testing)
confusionMatrix(predict.rpart, testing$classe)

## fitting with lda, accuracy = 0.6999
fit.lda = train(classe ~ ., data = training, method = "lda")
print(fit.lda$finalModel)
predict.lda = predict(fit.lda, newdata = testing)
confusionMatrix(predict.lda, testing$classe)

## fitting with naive bayes, too slow
##fit.nb = train(classe ~ ., data = training, method = "nb")
##print(fit.lda$finalModel)
##predict.lda = predict(fit.lda, newdata = testing)
##confusionMatrix(predict.lda, testing$classe)

## fitting with rf, accuracy = 0.9939
fit.rf = train(classe ~ ., data = training, method = "rf", importance=TRUE)
print(fit.rf$finalModel)
predict.rf = predict(fit.rf, newdata = testing)
confusionMatrix(predict.rf, testing$classe)
answers = predict(fit.rf, newdata = test_data)
dim(test_data)
## fitting with gbm, accuracy = 0.6999
fit.gbm = train(classe ~ ., data = training, method = "gbm")
print(fit.lda$finalModel)
predict.lda = predict(fit.lda, newdata = testing)
confusionMatrix(predict.lda, testing$classe)

if(!file.exists(file.path(getwd(), "submission")))
{
        dir.create(file.path(getwd(), "submission"))
}

pml_write_files = function(x,y){
        n = length(x)
        for(i in 1:n){
                filename = file.path(y, paste0("problem_id_",i,".txt"))
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(answers, file.path(getwd(), "submission"))

library("foreach")
library("doSNOW")
registerDoSNOW(makeCluster(4, type="SOCK"))

x <- matrix(runif(500), 100)
y <- gl(2, 50)

rf <- foreach(ntree = rep(250, 4), .combine = combine, .packages = "randomForest") %dopar%
        +    randomForest(x, y, ntree = ntree)
