#################### K Nearest Neighbor #################### 
library(kknn)
library(caret)
library(kableExtra)

#GB SetWD
source("6372_Project2_HOF/ImportData.R")

test.reduce <- test[,c(6, 34,37, 89, 100, 121)]

#Knn
#train[,c(cols.Inducted, cols.Batting.avg)]
set.seed(123)
#knn.train = train(Attrition~., data=emp_train[,c(col.CatKNN)], method="knn", trControl=control, tuneGrid=grid1)
knn.train = train(HallOfFame_inducted~., data=train[,c(cols.Inducted, cols.Batting)], method="knn")
knn.test = knn(
  train[, c(cols.Batting, cols.Batting.avg, cols.Awards)], 
  test[, c(cols.Batting, cols.Batting.avg, cols.Awards)], 
  train$HallOfFame_inducted, 
  k=1
)
confusionMatrix(table(test$HallOfFame_inducted, knn.test))

# K Weighted
set.seed(123)
kknn.train = train.kknn(HallOfFame_inducted~., data=train[,c(cols.Inducted, cols.KNNData)], kmax=30, distance = 2)
prediction <- predict(kknn.train, test[,c(cols.Inducted, cols.KNNData)][,-1])
kWeightedPrediction <- confusionMatrix(table(test[,c(cols.Inducted, cols.KNNData)][,1],prediction))
kWeightedPrediction <-confusionMatrix(table(knn.test, test$HallOfFame_inducted))
kWeightedPrediction
#graphics.off() 
#par(mar=c(5,5,5,5))
plot(kknn.train)


# Compare
dt0 <- data.frame(cbind(t(knnPrediction$overall),t(knnPrediction$byClass)))
dt0$Type <- as.character("kNN")
dt1 <- data.frame(cbind(t(kWeightedPrediction$overall),t(kWeightedPrediction$byClass)))
dt1$Type <- as.character("kWeighted")
SummaryPred <-rbind(dt0, dt1)
SummaryPred <- SummaryPred[order(-SummaryPred$Accuracy),]
SummaryPred <- SummaryPred[,c(19,1:18)]
SummaryPred  %>%   kable() %>%      kable_styling(bootstrap_options = "striped", full_width = F) %>% scroll_box(width = "700px", height = "200px")



bag.adv<-randomForest( HallOfFame_inducted ~ TV+radio,data=Adver , subset=index ,
                       mtry=2,importance =TRUE,ntree=100)

yhat.bag = predict (bag.adv , newdata=test)
plot(yhat.bag , test$sales,main="Bagged Model",xlab="Predicted",ylab="Test Set Sales")
abline (0,1)

