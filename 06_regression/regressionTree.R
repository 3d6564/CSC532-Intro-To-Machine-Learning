# Regression Tree Modeling
## Packages
library(rpart)
library(rpart.plot)
library(Cubist)
library(RWeka)
library(partykit)

## Data Import
wine <- read.csv("6_regression/data/whitewines.csv")
str(wine)
summary(wine)
attach(wine)

## Data Exploration
hist(wine$quality)

## Estimating quality model with 80% training set
smp_size <- floor(.80 * nrow(wine))
smp_size_plus <- smp_size+1
pop_size <- as.numeric(nrow(wine))

wine_train <- wine[1:smp_size,]
wine_test <- wine[smp_size_plus:pop_size,]

wine_rpart <- rpart(quality~., data=wine_train)
wine_rpart
#train_control <- trainControL(method="cv", number=10)
#wine_rpart <- train(quality~., data=wine,
#                    method="rpart",
#                    trControl=train_control)

## View Regression Tree model
rpart.plot(wine_rpart, digits=3)

## Evaluate Regression Tree model
predict_wine <- predict(wine_rpart, wine_test)

summary(predict_wine)

summary(wine_test$quality)

mean(abs(wine_test$quality-predict_wine))
sqrt(mean(abs(wine_test$quality-predict_wine)^2))

## Cubist model - Used instead of M5P model
### Algorithm created 32 rules to predict wine quality
### The model creates a separate regression model for each leaf node
wine_cubist <- cubist(x=wine_train[-12], y=wine_train$quality)
wine_cubist
summary(wine_cubist)

## Evaluate Cubist Regression model
predict_wine_cubist <- predict(wine_cubist, wine_test)
summary(predict_wine_cubist)
summary(wine_test$quality)

MAE <- mean(abs(wine_test$quality-predict_wine_cubist))
MAE
RMSE <- sqrt(mean(abs(wine_test$quality-predict_wine_cubist)^2))
RMSE