cat("\014")
rm(list=ls());
library(caret); library(randomForest); 
training<-read.csv("pml-training-small.csv");
testing<-read.csv("pml-testing-small.csv");

trainingSet<- training[sample(1:nrow(training), 10000, replace=FALSE),];
trainingSet<-trainingSet[,-c(1:7)];
testingSet<-testing[,-c(1:7)];

inTrain <- createDataPartition (y=trainingSet$classe, p=0.7, list = FALSE);
train70 <- trainingSet[inTrain,];
test30 <- trainingSet[-inTrain,];
ctrl = trainControl(method = "cv", number = 3);

modFit1 <- train(classe ~., method="gbm", trControl=ctrl, preProcess="pca", data=train70);
pred1<-predict(modFit1, testingSet);
print(pred1)

modFit2 <- train(classe ~., method="rf", trControl=ctrl, data=train70);
pred2<-predict(modFit2, testingSet);
print(pred2)

newtest<-training[sample(1:nrow(training), 20, replace=FALSE),];
newtest<-newtest[,-c(1:7)];

confusionMatrix(pred1, newtest$classe);
confusionMatrix(pred2, newtest$classe);

