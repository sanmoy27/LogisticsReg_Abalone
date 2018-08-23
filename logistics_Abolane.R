setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\Project\\data\\logistic")
library(MASS)
library(caTools)
library(ISLR)
library(glmnet)
library(caret)
library(class)

AbaloneData<-read.csv("Abalone.csv", header = TRUE, stringsAsFactors = FALSE)
dim(AbaloneData)
str(AbaloneData)
summary(AbaloneData)

AbaloneData$sex<-ifelse(AbaloneData$sex=='M', 1, 0)
AbaloneData$sex<-as.factor(AbaloneData$sex)

detectNAs<-function(x){
  return(sum(is.na(x)))
}
lapply(AbaloneData, detectNAs)


cat("\014")
cat("\014")
set.seed(100) # set seed to replicate results
split<-sample.split(AbaloneData$sex, SplitRatio=0.8)
trainSet<-subset(AbaloneData, split==TRUE)
testSet<-subset(AbaloneData, split==FALSE)


log.fit <- glm(sex ~ ., data=trainSet, family = "binomial")
summary(log.fit)

cat("\014")
log.fit1 <- glm(sex ~ (length+diameter+weight.w+weight.s+weight.v+weight.sh+rings), data=trainSet, family = "binomial")
summary(log.fit1)

cat("\014")
log.fit2 <- glm(sex ~ (length+diameter+weight.s+weight.v+weight.sh+rings), data=trainSet, family = "binomial")
summary(log.fit2)

cat("\014")
log.fit3 <- glm(sex ~ (length+weight.s+weight.v+weight.sh+rings), data=trainSet, family = "binomial")
summary(log.fit3)

cat("\014")
log.fit4 <- glm(sex ~ (weight.s+weight.v+weight.sh+rings), data=trainSet, family = "binomial")
summary(log.fit4)

cat("\014")
log.fit5 <- glm(sex ~ (weight.s+weight.sh+rings), data=trainSet, family = "binomial")
summary(log.fit5)

predict.fit<-predict(log.fit5, newdata = testSet, type="response")
conf1<-table(predicts=predict.fit>0.5, actuals=testSet$sex)
conf1

accuracy1=sum(diag(conf1))/sum(conf1)
accuracy1

precision=(469)/(469+257)
precision

recall=(469)/(469+61)
recall

pred <- ifelse(predict.fit >= 0.5, "Male", "Female") 
pred

ROCRpred <- prediction(predict.fit, testSet$sex)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
