#################### K Nearest Neighbor #################### 
library(kknn)
library(caret)
library(kableExtra)

#GB SetWD
source("6372_Project2_HOF/ImportData.R")
#test.reduce <- test[,c(6, 34,37, 89, 100, 121)]

#Knn
#train[,c(cols.Inducted, cols.Batting.avg)]
set.seed(123)
knn.train = train(HallOfFame_inducted~., data=train[,c(cols.Inducted, cols.Batting, cols.Awards_Key)], method="knn")
set.seed(123)
knn.test = knn(
  train[, c(cols.Batting, cols.Batting.avg, cols.Awards,cols.Awards_Key)], 
  test[, c(cols.Batting, cols.Batting.avg, cols.Awards, cols.Awards_Key)], 
  train$HallOfFame_inducted, 
  k=19
)
knnPrediction <- confusionMatrix(table(knn.test, test$HallOfFame_inducted))
knnPrediction

# knnPrediction$table  %>%   kable() %>%      kable_styling(bootstrap_options = "striped", full_width = F) %>% scroll_box(width = "700px", height = "200px")


# K Weighted
set.seed(123)
kknn.train = train.kknn(HallOfFame_inducted~., data=train[,c(cols.Inducted, cols.KNNData)], kmax=30, distance = 2)
prediction <- predict(kknn.train, test[,c(cols.Inducted, cols.KNNData)][,-1])
kWeightedPrediction <- confusionMatrix(table(test[,c(cols.Inducted, cols.KNNData)][,1],prediction))
kWeightedPrediction <-confusionMatrix(table(test$HallOfFame_inducted, knn.test))
kWeightedPrediction
#graphics.off() 
#par(mar=c(5,5,5,5))
plot(kknn.train)

# KNN With 1961 Data
set.seed(123)
knn.train.post.1961 = train(HallOfFame_inducted~., data=train.post.1961[,c(cols.Inducted, cols.Batting, cols.Awards_Key)], method="knn")
set.seed(123)
knn.prob = knn(
  train.post.1961[, c(cols.Batting, cols.Batting.avg, cols.Awards, cols.Awards_Key)], 
  test.post.1961[, c(cols.Batting, cols.Batting.avg, cols.Awards, cols.Awards_Key)], 
  train.post.1961$HallOfFame_inducted, 
  k=19,
  prob = TRUE
)

set.seed(123)
knn.test.post.1961 = knn(
  train.post.1961[, c(cols.Batting, cols.Batting.avg, cols.Awards, cols.Awards_Key)], 
  test.post.1961[, c(cols.Batting, cols.Batting.avg, cols.Awards, cols.Awards_Key)], 
  train.post.1961$HallOfFame_inducted, 
  k=19
)

test.post.1961$knn.prob <- attributes(knn.prob)$prob
test.post.1961$knn.predicted <- knn.prob
roccurve <- roc(response = test.post.1961$HallOfFame_inducted, predictor = test.post.1961$knn.prob)
plot(roccurve)
auc(roccurve)

kNNPost1961Prediction <- confusionMatrix(table(test.post.1961$knn.predicted, test.post.1961$HallOfFame_inducted))
kNNPost1961Prediction$table
kNNPost1961Prediction


auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

# Compare
dt0 <- data.frame(cbind(t(knnPrediction$overall),t(knnPrediction$byClass)))
dt0$Type <- as.character("kNN")
dt1 <- data.frame(cbind(t(kWeightedPrediction$overall),t(kWeightedPrediction$byClass)))
dt1$Type <- as.character("kWeighted")
dt2 <- data.frame(cbind(t(kNNPost1961Prediction$overall),t(kNNPost1961Prediction$byClass)))
dt2$Type <- as.character("kNNpost1961")
SummaryPred <-rbind(dt0, dt1, dt2)
SummaryPred <- SummaryPred[order(-SummaryPred$Accuracy),]
SummaryPred <- SummaryPred[,c(19,1:18)]
SummaryT <- SummaryPred[,c(1,2,9, 10)]
SummaryPred  %>%   kable() %>%      kable_styling(bootstrap_options = "striped", full_width = F) %>% scroll_box(width = "700px", height = "200px")
SummaryT  %>%   kable() %>%      kable_styling(bootstrap_options = "striped", full_width = F) %>% scroll_box(width = "700px", height = "200px")
knnPrediction$table  %>%   kable() %>%      kable_styling(bootstrap_options = "striped", full_width = F) %>% scroll_box(width = "700px", height = "200px")
kWeightedPrediction$table  %>%   kable() %>%      kable_styling(bootstrap_options = "striped", full_width = F) %>% scroll_box(width = "700px", height = "200px")
kNNPost1961Prediction$table  %>%   kable() %>%      kable_styling(bootstrap_options = "striped", full_width = F) %>% scroll_box(width = "700px", height = "200px")



bag.adv<-randomForest( HallOfFame_inducted ~ TV+radio,data=Adver , subset=index ,
                       mtry=2,importance =TRUE,ntree=100)

yhat.bag = predict (bag.adv , newdata=test)
plot(yhat.bag , test$sales,main="Bagged Model",xlab="Predicted",ylab="Test Set Sales")
abline (0,1)

names(train[,c(cols.Inducted, cols.Batting)])
