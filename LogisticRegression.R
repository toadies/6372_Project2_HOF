library(glmnet)
library(MASS)
library(caret)
library(pROC)
library(ROCR)
library(pheatmap)
library(randomForest)
library(class)
library(kknn)

train.full <- train[,c(cols.Inducted, cols.Batting.no.cor)]


#GB SetWD
source("6372_Project2_HOF/ImportData.R")
x <- model.matrix(HallOfFame_inducted~.,train.full)
#convert class to numerical variable
y <- ifelse(train$HallOfFame_inducted=="Y",1,0)

set.seed(1234)
glm.lasso <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse")
#plot result
plot(glm.lasso)

#min value of lambda
lambda_min <- glm.lasso$lambda.min
#best value of lambda
lambda_1se <- glm.lasso$lambda.1se
#regression coefficients
glm.lasso.coef <- coef(glm.lasso,s=lambda_1se)

# Rerun coef variables to remove penatlties caused by LASSO
data.frame(name = glm.lasso.coef@Dimnames[[1]][glm.lasso.coef@i + 1], coefficient = glm.lasso.coef@x)

# LASSO OUTPUT


# Get column indecis
cols.lasso.coef <- glm.lasso.coef@i
cols.lasso.coef <- cols.lasso.coef[-1] # Remove the intercept

train.reduce = train.full[,cols.lasso.coef]
# # Include doubles and HR to round out the slugging stat
# train.reduce$Batting_2B <- train$Batting_2B
# train.reduce$Batting_HR <- train$Batting_HR
names(train.reduce)

train.reduce$HallOfFame_inducted <- train.full$HallOfFame_inducted

glm.manual <- glm(HallOfFame_inducted~.,data = train.reduce, family = binomial)
summary(glm.manual)

glm.manual$coefficients
exp(cbind(coef(glm.manual), confint(glm.manual)))

dim(test)
lasso_prob <- predict.glm(glm.manual,test[,-cols.Inducted],type="response")

#predict class, type=”class”
# x.test <- model.matrix(HallOfFame_inducted~.,test)
# y.test <- ifelse(test$HallOfFame_inducted=="Y",1,0)
# lasso_prob <- predict(glm.lasso,newx = x.test, s=lambda_1se,type="response")

#translate probabilities to predictions
contrasts(test$HallOfFame_inducted)
lasso_predict <- rep("N",nrow(test))
lasso_predict[lasso_prob>.5] <- "Y"

# GB - Update Prediction on Traing Data to ensure we used right data set
# Me just playing this can be deleted
#contrasts(train$HallOfFame_inducted)
#lasso_predict <- rep("N",nrow(train))
#lasso_predict[lasso_prob>.5] <- "Y"


#confusion matrix (Updated)
# Confusion Variables are wrong...
#confusionMatrix(table(lasso_predict,lasso_predict))
cf <-confusionMatrix(table(test$HallOfFame_inducted, lasso_predict))
cf

# ROC Curves (Updated)
roccurve <- roc(test$HallOfFame_inducted ~ lasso_prob)
plot(roccurve)
auc(roccurve)

#################### K Nearest Neighbor #################### 

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