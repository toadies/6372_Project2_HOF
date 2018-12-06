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

t <- table(predicted=test.prd,actual=test.post.1961$HallOfFame_inducted)
fourfoldplot(t, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Figure 6 - LDA Confusion Matrix")


pred.lda <- predict(fit.lda, newdata = test.post.1961)
preds <- pred.lda$posterior
preds <- as.data.frame(preds)
pred <- prediction(preds[,2],test.lda.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.test <- performance(pred, measure = "auc")
auc.test <- auc.test@y.values
plot(roc.perf, main="Figure 7 - LDA ROC Curve")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.test[[1]],3), sep = ""))


fit.lda$prior
fit.lda$means
fit.lda$scaling
fit.lda$svd^2/sum(fit.lda$svd^2)

# Make a prediction on players retired from 2012 to 2015
result.recents$pred<-predict(fit.lda, newdata = result.recents)$class
table(result.recents$pred,result.recents$HallOfFame_inducted)

t(result.recents[result.recents$pred=="Y",c(19,20,100,cols.Batting,89)])




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
