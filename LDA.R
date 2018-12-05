source("6372_Project2_HOF/ImportData.R")

train.lda.x <- train.post.1961[,c(cols.Batting.no.cor, 89, 121)]
train.lda.y <- train.post.1961$HallOfFame_inducted
test.lda.y <- test.post.1961$HallOfFame_inducted

names(train.lda.x)
fit.lda <- lda(
            train.lda.y ~ .,
            data = train.lda.x)

#confusion matrix
train.prd<-predict(fit.lda, newdata = train.post.1961)$class
confusionMatrix(table(train.prd,train.post.1961$HallOfFame_inducted))

pred.lda <- predict(fit.lda, newdata = train.post.1961)
preds <- pred.lda$posterior
preds <- as.data.frame(preds)
pred <- prediction(preds[,2],train.lda.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

test.prd<-predict(fit.lda, newdata = test.post.1961)$class
confusionMatrix(table(test.prd,test.post.1961$HallOfFame_inducted))

pred.lda <- predict(fit.lda, newdata = test.post.1961)
preds <- pred.lda$posterior
preds <- as.data.frame(preds)
pred <- prediction(preds[,2],test.lda.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.test <- performance(pred, measure = "auc")
auc.test <- auc.test@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.test[[1]],3), sep = ""))


fit.lda$prior
fit.lda$means
fit.lda$scaling
fit.lda$svd^2/sum(fit.lda$svd^2)







## Entire Data Set
train.lda.x <- train[,c(cols.Batting.no.cor, 89, 121)]
train.lda.y <- train$HallOfFame_inducted

names(train.lda.x)
fit.lda <- lda(
  train.lda.y ~ .,
  data = train.lda.x)

#confusion matrix
train.prd<-predict(fit.lda, newdata = train)$class
confusionMatrix(table(train.prd,train$HallOfFame_inducted))

test.prd<-predict(fit.lda, newdata = test)$class
confusionMatrix(table(test.prd,test$HallOfFame_inducted))
