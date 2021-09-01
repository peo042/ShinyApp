library(readr)
library(caret)
library(faraway)
library(pROC)
library(tidyverse)
library(ROCR)
library(dplyr)
library(stats)
install.packages('stats')


Final<-read.csv('C:\\Users\\hoffm\\Documents\\Final.csv')
Final$Bike = as.factor(Final$Bike)
Final$TerritoryID = as.factor(Final$TerritoryID)
Final$HomeOwnerFlag = as.factor(Final$HomeOwnerFlag)

set.seed(12345)
training <- createDataPartition(Final$Bike, p=.8, list=F)

train <- Final[training, ]
test <- Final[-training, ]

glm.Bike <- train(form = Bike ~ TerritoryID+MaritalStatus+Education+Gender
                               +YearlyIncome+NumberChildrenAtHome+Occupation+TotalChildren
                               +HomeOwnerFlag+NumberCarsOwned+CommuteDistance+Age,
                 data = train, trControl = trainControl(method = "cv", number = 5), 
                 method = "glm", family = "binomial")

tree.Bike <-train(form = Bike~TerritoryID+MaritalStatus+Education+Gender
                  +YearlyIncome+NumberChildrenAtHome+Occupation+TotalChildren
                  +HomeOwnerFlag+NumberCarsOwned+CommuteDistance+Age, 
                  data=train, trControl= trainControl("cv", number = 10), 
                  method="rpart")

bag.Bike <- train(form = Bike~TerritoryID+MaritalStatus+Education+Gender
                  +YearlyIncome+NumberChildrenAtHome+Occupation+TotalChildren
                  +HomeOwnerFlag+NumberCarsOwned+CommuteDistance+Age, 
                  data=train, method = "rf", trControl=trainControl("cv", number = 10), importance=T)

trim.Bike <-train(form = Bike~TerritoryID+MaritalStatus
                  +NumberChildrenAtHome+TotalChildren+Education
                  +HomeOwnerFlag+NumberCarsOwned+CommuteDistance+Age, 
                  data=train, method = "rf", trControl=trainControl("cv", number = 10), importance=T)

glm.pred <- predict(glm.Bike, test)
tree.pred <- predict(tree.Bike, test)
bag.pred <- predict(bag.Bike, test)
trim.pred <- predict(trim.Bike, test)

confusionMatrix(glm.pred, test$Bike)
confusionMatrix(tree.pred, test$Bike)
confusionMatrix(bag.pred, test$Bike)
confusionMatrix(trim.pred, test$Bike)

varImp(glm.Bike)
varImp(tree.Bike)
varImp(bag.Bike)
varImp(trim.Bike)

glm.prob=predict(glm.Bike, newdata=test, type = 'prob')
tree.prob=predict(tree.Bike, newdata=test, type = 'prob')
bag.prob=predict(bag.Bike, newdata=test, type = 'prob')
trim.prob=predict(trim.bike, newdata=test, type = 'prob')

glm.roc=roc(response=test$Bike, predictor=glm.prob$'1')
tree.roc=roc(response=test$Bike, predictor=tree.prob$'1')
bag.roc=roc(response=test$Bike, predictor=bag.prob$'1')
trim.roc=roc(response=test$Bike, predictor=trim.prob$'1')

plot.roc(glm.roc, print.auc = TRUE, print.thres = "best")
plot.roc(tree.roc, print.auc = TRUE, print.thres = "best")
plot.roc(bag.roc, print.auc = TRUE, print.thres = "best")
plot.roc(trim.roc, print.auc = TRUE, print.thres = "best")

summary(Final[,-17])
