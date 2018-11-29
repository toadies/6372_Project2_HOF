#################### K Nearest Neighbor #################### 

#GB SetWD
source("6372_Project2_HOF/ImportData.R")

#Knn
#train[,c(cols.Inducted, cols.Batting.avg)]
set.seed(123)
#knn.train = train(Attrition~., data=emp_train[,c(col.CatKNN)], method="knn", trControl=control, tuneGrid=grid1)
knn.train = train(HallOfFame_inducted~., data=train[,c(cols.Inducted, cols.KNNData)], method="knn")
knn.test = knn(train[,c(cols.Inducted, cols.KNNData)][,-1], test[,c(cols.Inducted, cols.KNNData)][,-1], train[,c(cols.Inducted, cols.KNNData)][,1], k=27)
knnPrediction <-confusionMatrix(table(knn.test, test$HallOfFame_inducted))
knnPrediction

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