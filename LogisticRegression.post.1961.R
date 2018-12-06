library(glmnet)
library(MASS)
library(caret)
library(pROC)
library(ROCR)
library(pheatmap)
library(randomForest)
library(class)
library(kknn)
library(dplyr)

#GB SetWD
source("6372_Project2_HOF/ImportData.R")

# Create a very simple model to if # Hits is better than our final model
glm.simple <- glm(HallOfFame_inducted~Batting_H,data = train.post.1961, family = binomial)
summary(glm.simple)
# Test Model
test.post.1961$simple.prob <- predict.glm(glm.simple,test.post.1961[,-cols.Inducted],type="response")
test.post.1961$simple.predicted <- ifelse(test.post.1961$simple.prob>.5,"Y","N")
confusionMatrix(table(test.post.1961$simple.pred, test.post.1961$HallOfFame_inducted))

# Let's get our data for the LASSO
train.post.1961.full <- train.post.1961[,c(cols.Inducted, cols.Batting, cols.Batting.avg, cols.Awards)]
names(train.post.1961.full)

x <- model.matrix(HallOfFame_inducted~.,train.post.1961.full)
y <- ifelse(train.post.1961$HallOfFame_inducted=="Y",1,0)

set.seed(123)
glm.lasso <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse")
plot(glm.lasso)

#min value of lambda
lambda_min <- glm.lasso$lambda.min
#best value of lambda
lambda_1se <- glm.lasso$lambda.1se
#regression coefficients
glm.lasso.coef <- coef(glm.lasso,s=lambda_1se)
data.frame(name = glm.lasso.coef@Dimnames[[1]][glm.lasso.coef@i + 1], coefficient = glm.lasso.coef@x)

# Get column indecis
cols.lasso.coef <- glm.lasso.coef@i
cols.lasso.coef <- cols.lasso.coef[-1] # Remove the intercept

train.post.1961.reduce = train.post.1961.full[,cols.lasso.coef]
train.post.1961.reduce$HallOfFame_inducted <- train.post.1961.full$HallOfFame_inducted
dim(train.post.1961.reduce)
#Assess Model
glm.manual <- glm(HallOfFame_inducted~.,data = train.post.1961.reduce, family = binomial)
summary(glm.manual)

# Alot of Awards that are not statistically significant.  Do some EDA
# Remove Hutch Award and Player of the Year Award, to many zeros
playerColIndex <- which(colnames(train.post.1961.reduce)=="Awards_TSNMajorLeaguePlayerOfTheYear")
train.post.1961.reduce <- train.post.1961.reduce[,-playerColIndex]
names(train.post.1961.reduce)
hutchColIndex <- which(colnames(train.post.1961.reduce)=="Awards_HutchAward")
train.post.1961.reduce <- train.post.1961.reduce[,-hutchColIndex]
names(train.post.1961.reduce)

# Rerun
glm.manual <- glm(HallOfFame_inducted~.,data = train.post.1961.reduce, family = binomial)
summary(glm.manual)

# Awards still suck
# Remove all of them!
goldGColIndex <- which(colnames(train.post.1961.reduce)=="Awards_GoldGlove")
train.post.1961.reduce <- train.post.1961.reduce[,-goldGColIndex]
names(train.post.1961.reduce)
allStarGColIndex <- which(colnames(train.post.1961.reduce)=="Awards_TSNAllStar")
train.post.1961.reduce <- train.post.1961.reduce[,-allStarGColIndex]
names(train.post.1961.reduce)

# Rerun
glm.final <- glm(HallOfFame_inducted~.,data = train.post.1961.reduce, family = binomial)
summary(glm.final)

glm.final$coefficients
exp(cbind(coef(glm.final), confint(glm.final)))

# Test Model
train.post.1961$final.prob <- predict.glm(glm.final,train.post.1961[,-cols.Inducted],type="response")
train.post.1961$final.predicted <- ifelse(train.post.1961$final.prob>.5,"Y","N")
confusionMatrix(table(train.post.1961$final.predicted, train.post.1961$HallOfFame_inducted))

test.post.1961$final.prob <- predict.glm(glm.final,test.post.1961[,-cols.Inducted],type="response")
test.post.1961$final.predicted <- ifelse(test.post.1961$final.prob>.5,"Y","N")
confusionMatrix(table(test.post.1961$final.predicted, test.post.1961$HallOfFame_inducted))

result$final.prob <- predict.glm(glm.final,result[,-cols.Inducted],type="response")
result$final.predicted <- ifelse(result$final.prob>.5,"Y","N")
confusionMatrix(table(result$final.predicted, result$HallOfFame_inducted))


# Positions don't matter
train.post.1961.positions <- train.post.1961.reduce
train.post.1961.positions$position.c <- train.post.1961$position.c
train.post.1961.positions$position.1b <- train.post.1961$position.1b
train.post.1961.positions$position.2b <- train.post.1961$position.2b
train.post.1961.positions$position.3b <- train.post.1961$position.3b
train.post.1961.positions$position.ss <- train.post.1961$position.ss
train.post.1961.positions$position.lf <- train.post.1961$position.lf
train.post.1961.positions$position.rf <- train.post.1961$position.rf
train.post.1961.positions$position.cf <- train.post.1961$position.cf

# Rerun
glm.positions <- glm(HallOfFame_inducted~.
                        #  Batting_3B+
                        # AllstarGames
                         ,data = train.post.1961.positions, family = binomial)
summary(glm.positions)
# positions don't matter

# Check LogisticRegression.post.1961.R for new analysis

# Not players played before 1961.  Create a new model that will focus on players played after 1961

# Compare to original model
glm.post.1961.final <- glm(HallOfFame_inducted~
                             Batting_R+
                             Batting_3B+
                             Batting_Average+
                             AllstarGames+
                             TotalAllStarAwards+
                             position.c
                           ,data = train.post.1961, family = binomial)
summary(glm.post.1961.final)

glm.post.1961.final <- glm(HallOfFame_inducted~
                             Batting_R+
                             Batting_3B+
                             AllstarGames
                           ,data = train.post.1961, family = binomial)
summary(glm.post.1961.final)

# Test Model
test.post.1961$final.prob <- predict.glm(glm.post.1961.final,test.post.1961[,-cols.Inducted],type="response")
test.post.1961$final.predicted <- ifelse(test.post.1961$final.prob>.5,"Y","N")
confusionMatrix(table(test.post.1961$final.predicted, test.post.1961$HallOfFame_inducted))
roccurve <- roc(response = test.post.1961$HallOfFame_inducted, predictor = test.post.1961$final.prob)
auc(roccurve)

train.post.1961$final.prob <- predict.glm(glm.post.1961.final,train.post.1961[,-cols.Inducted],type="response")
train.post.1961$final.predicted <- ifelse(train.post.1961$final.prob>.5,"Y","N")
confusionMatrix(table(train.post.1961$final.predicted, train.post.1961$HallOfFame_inducted))
roccurve <- roc(response = train.post.1961$HallOfFame_inducted, predictor = train.post.1961$final.prob)
auc(roccurve)

# No interaction terms helps by position.  Including Catcher alone improves the model
glm.post.1961.final$coefficients
View(exp(cbind(coef(glm.post.1961.final), confint(glm.post.1961.final))))

odds.multiplier <- c(1, 300, 30, 3)
View(exp(coef(glm.post.1961.final) * odds.multiplier ) )
     
car::vif(glm.post.1961.final)
round( 1 - ( glm.post.1961.final$deviance / glm.post.1961.final$null.deviance ), 2 )

ggplot( test.post.1961, aes( final.prob, color = HallOfFame_inducted ) ) + 
  geom_density( size = 1 ) +
  ggtitle( "Training Set's Predicted Score" ) + 
  theme(legend.position="none")

plot(glm.post.1961.final, which = 4, id.n = 3)
glm.post.1961.final.positions.data <- augment(glm.post.1961.final) %>% 
  mutate(index = 1:n()) 

glm.post.1961.final.positions.data %>% top_n(3, .cooksd)

residualsPlot <- ggplot(glm.post.1961.final.positions.data, aes(index, .std.resid)) + 
  geom_point(aes(color = HallOfFame_inducted), alpha = .5) + 
  theme(legend.position="none") +
  ggtitle("Model 2 Residual Plot")
residualsPlot
ggsave("6372_Project2_HOF/Residuals - Model 2.png",plot = residualsPlot, type = png())



source("6372_Project2_HOF/unbalanced_functions.r")

length(train.post.1961$final.prob)
length(train.post.1961$HallOfFame_inducted)

accuracy_info <- AccuracyCutoffInfo( 
  train.post.1961$final.prob,
  train.post.1961$HallOfFame_inducted,
  test.post.1961$final.prob,
  test.post.1961$HallOfFame_inducted
)
# define the theme for the next plot

accuracy_info$plot


car::vif(glm.final)
