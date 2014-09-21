if(!file.exists(file.path(getwd(), "data2")))
{
        dir.create(file.path(getwd(), "data2"))

}


subset(raw_training, select = names(raw_training) %like% "mb")
sort(names(raw_training))
raw_training[colnames(raw_training)]
names(raw_training[,grep('max', names(raw_training))])
names(raw_training[,grep('min', names(raw_training))])
names(raw_training[,grep('var', names(raw_training))])
names(raw_training[,grep('stddev', names(raw_training))])
names(raw_training[,grep('avg', names(raw_training))])
names(raw_training[,grep('total', names(raw_training))])
small_training = raw_training[,!(names(raw_training[,grep('max', names(raw_training))]))]
list_of_names = vector()
list_of_names = c(list_of_names, names(raw_training[,grep('max', names(raw_training))]))
list_of_names = c(list_of_names, names(raw_training[,grep('min', names(raw_training))]))
list_of_names = c(list_of_names, names(raw_training[,grep('var', names(raw_training))]))
list_of_names = c(list_of_names, names(raw_training[,grep('stddev', names(raw_training))]))
list_of_names = c(list_of_names, names(raw_training[,grep('avg', names(raw_training))]))
list_of_names = c(list_of_names, names(raw_training[,grep('total', names(raw_training))]))

small_training = raw_training[,!(colnames(raw_training) %in% list_of_names)]
dim(small_training)

names(small_training)

small_predictors = vector()
for(i in names(small_training[,1:length(small_training)-1]))
{        
        small_training[,i] = as.numeric(small_training[,i])
        if (sum(is.na(small_training[,i]))/length(small_training[,i]) < 0.5)
        {                
                small_predictors = rbind(small_predictors, i)
        }
}

small_training_clean = subset(small_training, select = c(small_predictors, "classe"))
summary(small_training_clean)
rpart = train(classe ~ .-X-user_name-raw_timestamp_part_1-raw_timestamp_part_2-cvtd_timestamp-new_window-num_window, method = "rpart", data = small_training_clean)
rpart.pred = predict(rpart)
confusionMatrix(rpart.pred,small_training_clean$classe )

library(doSNOW)
rf = train(classe ~ .-X-user_name-raw_timestamp_part_1-raw_timestamp_part_2-cvtd_timestamp-new_window-num_window, 
           method = "rf", data = small_training_clean, importance=TRUE)
install.packages("doSNOW")

plot(predict.rf, testing$classe)
roc(predict.rf, testing$classe)
varImpPlot(fit.rf$finalModel)
library(ROCR)
perf = performance(predict.rf, "tpr", "fpr")
result = prediction(rep("A", length(testing$classe)), testing$classe)
roc(predict.rf, as.numetesting$classe)
roc(rep(1:5), rep(5:9))
str(testing$classe)
str(predict.rf)
