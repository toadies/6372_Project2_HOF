source("6372_Project2_HOF/ImportData.R")

train.lda.x <- train[,c(cols.Batting, cols.Batting.avg)]
train.lda.y <- train[,cols.Inducted]
str(train[,c(cols.Inducted, cols.Batting, cols.Batting.avg, cols.Awards)])

fit.lda <- lda(
            train.lda.y ~ .,
            data = train.lda.x)
pred.lda <- predict(fit.lda, newdata = train)

preds <- pred.lda$posterior
preds <- as.data.frame(preds)

#confusion matrix
prd<-predict(fit.lda, newdata = test)$class
table(prd,test$HallOfFame_inducted)

pred <- prediction(preds[,2],train.lda.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


