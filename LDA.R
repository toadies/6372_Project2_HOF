source("6372_Project2_HOF/ImportData.R")

train.lda.x <- train.post.1961[,c(cols.Batting.no.cor)]
train.lda.y <- train.post.1961[,cols.Inducted]

fit.lda <- lda(
            train.lda.y ~ .,
            data = train.lda.x)
pred.lda <- predict(fit.lda, newdata = train)

preds <- pred.lda$posterior
preds <- as.data.frame(preds)

#confusion matrix
prd<-predict(fit.lda, newdata = test)$class
confusionMatrix(table(prd,test$HallOfFame_inducted))


pred <- prediction(preds[,2],train.lda.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


