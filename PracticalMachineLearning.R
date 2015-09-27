setwd("~/git/practical_machine_learning/")
pml_train <- read.csv("pml-training.csv")
pml_test <- read.csv("pml-testing.csv")

# find out the columns that are unavailable in the testing set
unmeasured <- which(colSums(is.na(pml_test))==20)
# do not use these columns in the testing set
pml_train <- pml_train[,-unmeasured]
# eliminate id number to avoid overfitting
pml_train <- pml_train[,-1]

head(pml_train)
dim(pml_train)

library(AppliedPredictiveModeling)
library(caret)
inTrain = createDataPartition(pml_train$classe, p = 3/4)[[1]]
training <- pml_train[inTrain,]
testing <- pml_train[-inTrain,]

mymodel = train(classe~., method='rf', data=training, 
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE, ntree = 50)
summary(mymodel)
plot(mymodel)

predicted_class <- predict(mymodel, newdata = testing)
accuracy <- sum(predicted_class == testing$classe)/length(predicted_class)
pml_test <- read.csv("pml-testing.csv")
answers <- predict(mymodel, newdata = pml_test)

#########################
# write out the answer
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
