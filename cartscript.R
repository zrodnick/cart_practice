library(rpart)
library(rpart.plot)
library(caTools)
stevens <- read.csv("stevens.csv")
set.seed(3000)
split <- sample.split(stevens$Reverse, SplitRatio=.7)
Test <- subset(stevens, split==FALSE)
Train <- subset(stevens, split==TRUE)

StevensTree <- rpart(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=Train, method="class", control=rpart.control(minbucket=25))
prp(StevensTree)
PredictCART <- predict(StevensTree, newdata=Test, type="class")
table(Test$Reverse, PredictCART)
library(ROCR)
PredictROC <- predict(StevensTree, newdata=Test)
pred <- prediction(PredictROC[,2], Test$Reverse)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

library(randomForest)

Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

StevensForest <- randomForest(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=Train, nodesize=25, ntree=200)
PredictForest <- predict(StevensForest, newdata=Test)
table(Test$Reverse, PredictForest)
library(caret)
library(e1071)

fitControl <- trainControl(method="cv", number=10)
cartGrid <- expand.grid(.cp=(1:50)*0.01)
train(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=Train, method="rpart", trControl=fitControl, tuneGrid=cartGrid)

StevensTreeCV <- rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, method="class", data=Train, control=rpart.control(cp=0.17))
PredictCV=predict(StevensTreeCV, newdata=Test, type="class")

table(Test$Reverse, PredictCV)
