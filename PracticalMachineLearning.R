setwd("~/git/practical_machine_learning/")
pml_train <- read.csv("pml-training.csv")
head(pml_train)
dim(pml_train)

library(AppliedPredictiveModeling)
library(caret)
inTrain = createDataPartition(pml_train$classe, p = 3/4)[[1]]
training <- pml_train[inTrain,]
testing <- pml_train[-inTrain,]

mymodel = train(classe~., method='rf', data=training, trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE, ntree = 10)
summary(mymodel)
plot(mymodel)

predicted_class <- predict(mymodel, newdata = testing)
pml_test <- read.csv("pml-testing.csv")
answer <- predict(mymodel, newdata = pml_test)
