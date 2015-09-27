
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
attach(segmentationOriginal)
training <- segmentationOriginal[Case=="Train",]
testing <- segmentationOriginal[Case=="Test",]
set.seed(125)
mymodel = train(Class~., method='rpart', data=training)

a = data.frame(TotalIntenCh2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1=2)
b =data.frame(TotalIntenCh2 = 50000, FiberWidthCh1 = 10,VarIntenCh4 = 100)
c  = data.frame(TotalIntenCh2 = 57000, FiberWidthCh1 = 8,VarIntenCh4 = 100) 
d = data.frame( FiberWidthCh1 = 8,VarIntenCh4 = 100, PerimStatusCh1=2)

new_data = merge(merge(merge(a,b,all=T),c,all = T),d,all = T)
all_data = merge(testing,new_data,all =F)
all_data[1011:1014,]

predict(mymodel, newdata=new_data)

################
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

set.seed(33833)
rfFit <- train(factor(y)~., data = vowel.train, method = 'rf')
gbmFit <- train(factor(y)~., data = vowel.train, method = 'gbm', verbose=F)

rfPredict = predict(rfFit, newdata = vowel.test)
gmbPredict = predict(gbmFit, newdata = vowel.test)

##################
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
rfFit <- train(diagnosis~., data = training, method = 'rf')
gbmFit <- train(diagnosis~., data = training, method = 'gbm', verbose=F)
ldaFit <- train(diagnosis~., data = training, method = 'lda', verbose=F)

rfPredict = predict(rfFit, newdata = testing)
gmbPredict = predict(gbmFit, newdata = testing)
ldaPredict = predict(ldaFit, newdata = testing)

allPredict = data.frame(rfPredict,gmbPredict,ldaPredict,testing$diagnosis)
stackingModel = train(testing.diagnosis~.,data = allPredict,method = 'rf')
compPredict = predict(stackingModel,newdata = allPredict)

sum(compPredict==testing$diagnosis)/length(testing$diagnosis)
sum(rfPredict==testing$diagnosis)/length(testing$diagnosis)
sum(gmbPredict==testing$diagnosis)/length(testing$diagnosis)
sum(ldaPredict==testing$diagnosis)/length(testing$diagnosis)

##
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
myfit<- enet(y = training$CompressiveStrength,x=training[,-9])

op <- par(mfrow=c(1, 1))
plot(myfit, "penalty",   label=TRUE)
plot(myfit, "L1norm", label=TRUE)

##
library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr, )

library(forecast)
fit <- bats(tstrain)
plot(forecast(fit)); lines(ts(testing$visitsTumblr), col="red")
