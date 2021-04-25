# Regression modeling
## Packages
library(psych)
library(caret)
library(leaps)

## Data Import
advertising <- read.csv("6_regression/data/Advertising.csv")
insurance <- read.csv("6_regression/data/insurance.csv", stringsAsFactors=TRUE)

## Data Exploration
### Advertising dataset
attach(advertising)
str(advertising)
summary(advertising)
plot(x=advertising$TV, y=advertising$sales, main="Scatterplot of Sales vs Tv",
     xlab="Money spent on TV ad (1000 Dollars)",
     ylab="Sales (1000 Dollars)")
cor(advertising$TV, advertising$sales)

### Insurance dataset
str(insurance)
summary(insurance)

## Simple Regression
### lm(outcome variable ~ feature, dataset)
### alpha hat = 7.03 and beta hat = .048
advertising.lm <- lm(sales ~ TV, data=advertising)

### Calculate alpha hat and beta hat
coefficients(advertising.lm)

### Plot alpha and beta line
abline(7.03259355, .04753664, col="red")
detach(advertising)

## Multiple Regression
### Correlation Matrix
#### Possible linear trend between age and expenses
attach(insurance)
cor(insurance[c("age", "bmi", "children", "expenses")])

### Correlation Matrix as plot
pairs(insurance[c("age", "bmi", "children", "expenses")])
pairs(~age+bmi+children+expenses, data=insurance)

### Scatterplot matrix for correlation
#### Correlation Coefficients shown above, scatterplots below. Diagonal shows
#### histograms for distribution of each variable
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

### Side by Side Boxplots
#### 2-Sided for a 2 level factor
#### Difference between mean of male/female IS NOT significant using alpha=.01
plot(expenses~sex, col="red")
t.test(expenses~sex, alternative="two.sided")

#### Difference between mean of smoker/nonsmoker IS significant using alpha=.01
plot(expenses~smoker, col="green")
t.test(expenses~smoker, alternative="two.sided")

#### Nova Oneway-test for multi-level factor
#### Difference between mean of regions IS NOT significant using alpha=.01
plot(expenses~region, col="blue")
oneway.test(expenses~region)

### Multiple regression model
#### More stars indicates higher statistical significance. This does not mean 
#### it will effect the outcome significantly. Higher R-Squared is a better fit.
#### Adjusted R-Squared is better for more features, it is penalized for each
#### feature.
ins_model <- lm(expenses~age+children+bmi+sex+smoker+region, data=insurance)
summary(ins_model)

### Evaluate performance of Linear Regression model with cross-validation
#### Caret package, cv is cross-validation, 10 is number of folds
train.control <- trainControl(method="cv", number=10)
ins_model_kf <- train(expenses~., 
                         data=insurance, 
                         method="lm", 
                         trControl=train.control)
ins_model_kf
summary(ins_model_kf)

### Improve model performance
#### convert numeric variable to a binary indicator
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

#### add nonlinearity effects
insurance$age2 <- insurance$age^2

#### new model
#### bmi30*smoker is taking into account high bmi individuals that also smoke
#### new model features lowered the RMSE and MAE, and increased R-Squared to .86
ins_model_kf2 <- train(expenses~age+age2+children+bmi+sex+bmi30*smoker+region,
                       data=insurance,
                       method="lm",
                       trControl=train.control)
ins_model_kf2
summary(ins_model_kf2)

## Stepwise Regression
### Library Leaps

### Regression model using the Backward method of feature selection
train.control <- trainControl(method="cv", number=10)
ins_model_stepB <- train(expenses~.,
                        data=insurance,
                        method="leapBackward",
                        trControl=train.control)
ins_model_stepB
#### * indicates if the variable was chosen, NVMAX=4 used children, smokeryes, 
#### bmi30, and age2
summary(ins_model_stepB$finalModel)

#### Compare to Fordward method... no significant change
ins_model_stepF <- train(expenses~.,
                         data=insurance,
                         method="leapForward",
                         trControl=train.control)
ins_model_stepB