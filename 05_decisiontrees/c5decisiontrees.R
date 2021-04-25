# C5.0 Decision Tree Algorithm

## Packages
library(C50)
library(gmodels)

## Data Load
credit <- read.csv("5_decisiontrees/data/credit.csv")
attach(credit)

## Data Exploration
str(credit)
summary(credit)

lapply(credit[,sapply(credit,is.factor)], function(x) table(x))
summary(credit[,sapply(credit,is.integer)])

## Assign seed
RNGversion("3.5.2"); set.seed(123)

## Train and test subsets
smp_train <- sample(1000, 900)

credit_train <- credit[smp_train,-17]
credit_test <- credit[-smp_train,-17]

credit_train_labels <- credit[smp_train,17]
credit_test_labels <- credit[-smp_train,17]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Build Model
credit_model <- C5.0(credit_train, credit_train_labels, trials=1)

## View decision tree
credit_model
summary(credit_model)

## Evaluate Model
### FPR of 24% and FNR of 36%, overall error 27%
credit_pred <- predict(credit_model, credit_test)
CrossTable(credit_pred, credit_test_labels, prop.chisq=FALSE, prop.t=FALSE,
           dnn=c("predicted","actual"))

## Improve Performance
### FPR of 17% and FNR of 20%, overall error of 18%. Correctly predicted 
### defaults is only 60.6%. 
credit_b10 <- C5.0(credit_train, credit_train_labels, trials=10)
credit_b10_pred <- predict(credit_b10, credit_test)

CrossTable(credit_b10_pred, credit_test_labels, prop.chisq=FALSE, prop.t=FALSE,
           dnn=c("predicted","actual"))

## Cost Matrices to penalize certain types of errors
matrix_dim <- list(c("no","yes"), c("no", "yes"))
names(matrix_dim) <- c("predicted", "actual")
error_cost <- matrix(c(0,1,4,0), nrow=2, dimnames=matrix_dim)

## Improve Performance - Cost matrix
### FPR of 24% and FNR of 23%, overall 24%. Using an error matrix in this case
### is worse than not using one. 
credit_cost <- C5.0(credit_train, credit_train_labels, 
                    costs=error_cost, trials=10)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_cost_pred, credit_test_labels, prop.chisq=FALSE, prop.t=FALSE,
           dnn=c("predicted","actual"))
