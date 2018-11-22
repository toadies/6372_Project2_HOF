library(glmnet)
library(MASS)
library(caret)
library(pROC)

source("6372_Project2_HOF/ImportData.R")

x <- model.matrix(HallOfFame_inducted~.,train[,c(cols.Inducted, 3, cols.Batting.avg, cols.Fielding)])
#convert class to numerical variable
y <- ifelse(train$HallOfFame_inducted=="Y",1,0)
#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso
# check docs to explore other type.measure options
set.seed(1234)
glm.lasso <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse", nfolds=10 )
#plot result
plot(glm.lasso)

#min value of lambda
lambda_min <- glm.lasso$lambda.min
#best value of lambda
lambda_1se <- glm.lasso$lambda.1se
#regression coefficients
glm.lasso.coef <- coef(glm.lasso,s=lambda_1se)
View(data.frame(name = glm.lasso.coef@Dimnames[[1]][glm.lasso.coef@i + 1], coefficient = glm.lasso.coef@x))

#predict class, type=”class”
x.test <- model.matrix(HallOfFame_inducted~.,test[,c(cols.Inducted, 3, cols.Batting.avg, cols.Fielding)])
y.test <- ifelse(test$HallOfFame_inducted=="Y",1,0)
lasso_prob <- predict(glm.lasso,newx = x.test, s=lambda_1se,type="response")

#translate probabilities to predictions
contrasts(test$HallOfFame_inducted)
lasso_predict <- rep("N",nrow(test))
lasso_predict[lasso_prob>.5] <- "Y"
#confusion matrix

confusionMatrix(table(lasso_predict,test$HallOfFame_inducted))

# Still need to fix
roccurve <- roc(y.test ~ lasso_prob)
plot(roccurve)
auc(roccurve)
# 
# 
# 
# glm.manual <- glm(HallOfFame_inducted~
#                       Batting_total_teams+
#                       Batting_R+
#                       Batting_3B+
#                       Batting_RBI+
#                       # Fielding_E+ corresponds with # games
#                       Awards_BaseballMagazineAllStar+
#                       Awards_TSNAllStar+
#                       Awards_BabeRuthAward+
#                       # Awards_HankAaronAward+ #Exlude, coef to small to measure
#                       AllstarGames
#                   ,data = result, family = binomial)
# c <- summary(glm.manual)
# 
# View(glm.manual$coefficients)
# View(exp(cbind(coef(glm.manual), confint(glm.manual))))
# result$lasso_prob <- predict.glm(glm.manual,newx = result)

