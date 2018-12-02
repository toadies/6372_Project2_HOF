source("6372_Project2_HOF/ImportData.R")

train.lda.x <- train.post.1961[,c(cols.Batting.no.cor, 89, 121)]
train.lda.y <- train.post.1961$HallOfFame_inducted

names(train.lda.x)
fit.lda <- lda(
            train.lda.y ~ .,
            data = train.lda.x)

#confusion matrix
test.prd<-predict(fit.lda, newdata = test.post.1961)$class
confusionMatrix(table(test.prd,test.post.1961$HallOfFame_inducted))

train.prd<-predict(fit.lda, newdata = train.post.1961)$class
confusionMatrix(table(train.prd,train.post.1961$HallOfFame_inducted))

fit.lda$prior
fit.lda$means
fit.lda$scaling
fit.lda$svd^2/sum(fit.lda$svd^2)

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
