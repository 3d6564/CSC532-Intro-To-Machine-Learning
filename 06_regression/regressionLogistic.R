# Logistic Regression Modeling
## Packages
library(ISLR)
library(psych)

## Data exploration
str(Smarket)
summary(Smarket)

### Correlations are close to zero between lag features and Today returns
pairs.panels(Smarket[-9])

attach(Smarket)

## Plotting
### plots show no major difference in means
par(mfcol=c(2,3))   #this line creates a 2*3 plotting window
plot(Lag1 ~ Direction,main="lag1", col="red")
plot(Lag2 ~ Direction,main="lag2", col="red")
plot(Lag3 ~ Direction,main="lag3", col="red")
plot(Lag4 ~ Direction,main="lag4", col="red")
plot(Lag5 ~ Direction,main="lag5", col="red")
plot(Volume ~ Direction,main="Volume", col="red")

## boolean vector of year
inTrain <- Year<2005 

## Logistic modeling
smarket_train <- Smarket[inTrain,]
smarket_test <- Smarket[!inTrain,]
dim(smarket_train)
dim(smarket_test)

smarket_logistic <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Volume,
                        data=smarket_train,
                        family="binomial")
### Note the very large p-values, so even though lag1 means if the market has a
### positive return yesterday then it is likely to go up today, it is not 
### significant because p-value > .05
summary(smarket_logistic)

### model is uncertain due to probabilities being 50-52%
predict_smarket <- predict(smarket_logistic, smarket_test, type="response")
head(predict_smarket)

predicted_label <- factor(ifelse(predict_smarket>.5, "Up", "Down"))
actual_label <- Direction[!inTrain]
t <- table(predicted_label, actual_label)
t

## Calculate model error
### test error is ~51.9% which means the model is not really useful
smarket_error=(t[1,2]+t[2,1])/sum(t)
smarket_error
