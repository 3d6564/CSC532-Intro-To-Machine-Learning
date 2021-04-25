# Lazy Learning with KNN
## Packages
library(class)
library(gmodels)
library(caret)

## Data Import from Wisconsin Breast cancer Diagnostic dataset
wbcd = read.csv("3_lazylearning/data/wisc_bc_data.csv")

## Data exploration and Prep
str(wbcd)

### drop patient ID
wbcd <- wbcd[-1] 

### re-label B and M and Benign and Malignant
wbcd$diagnosis <- factor(wbcd$diagnosis, levels=c("B", "M"),
                         labels=c("Benign", "Malignant"))
table(wbcd$diagnosis)
round(prop.table(table(wbcd$diagnosis)) * 100, digits=1)

### Validate scale of numeric variables, scales are significantly different
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

### Further Data exploration
attach(wbcd)
plot(radius_mean~diagnosis, col="red")
plot(texture_mean~diagnosis, col="red")

### p-value is <.01 thus the difference between mean is significant
t.test(radius_mean~diagnosis, alternative="two.sided")
t.test(texture_mean~diagnosis, alternative="two.sided")

## Function to normalize numeric data
normalize <- function(x) {
  return ((x-min(x)) / (max(x)-min(x)))
}

## Normalize data
wbcd <- wbcd[sample(nrow(wbcd), replace=FALSE), ] # randomly shuffle data
wbcd_n <- as.data.frame(sapply(wbcd[2:31], normalize)) # normalize
summary(wbcd_n) # verify scale adjusted

## Train and Test subsets
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569, ]

## Diagnosis labels from main data set
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

## KNN classifying data
wbcd_test_pred <- knn(train=wbcd_train, 
                      test=wbcd_test, 
                      cl=wbcd_train_labels,
                      k=21) # square root of data points in test set

## Evaluate the algorithm performance
### Both vectors are nomial and thus can use a CrossTab. CrossTab shows 62+37 or
### 99 out of 100 were categorized correctly. 1 incorrectly classed as benign
### when it was malignant.
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)

## K-fold cross validation
### create folds
folds <- createFolds(wbcd$diagnosis, k=10)
str(folds)

### k fold function
### Creates train set, validation set, train labels, validation labels, and
### then prediction set using the knn() function. Then compare predicted labels
### with true labels and send to table. Error is a calculation to get a 
### percentage for the error of misdiagnosis, finding a ratio from the table.
knn_fold <- function(features, target, fold, k) {
  train=features[-fold, ]
  validation=features[fold, ]
  train_labels=target[-fold]
  validation_labels=target[fold]
  validation_preds=knn(train, validation, train_labels, k=k)
  t=table(validation_labels, validation_preds)
  error=(t[1,2]+t[2,1]) / (t[1,1]+t[1,2]+t[2,1]+t[2,2])
  return(error)
}

knn_fold(wbcd_n, wbcd$diagnosis, folds[[1]], k=21)

### cross validation function
### creates folds, and passes all required variables such as features, target, 
### and k value to the knn_fold and applies to each point.

### This creates 10 folds, and then does the knn_fold function. This stores the
### errors in errors variable and then will return the mean of errors.
crossValidationError <- function(features, target, k) {
  folds=createFolds(target, k=10)
  errors=sapply(folds, knn_fold, features=features, target=target, k=k)
  return(mean(errors))
}

### call cross validation function
crossValidationError(wbcd_n, wbcd$diagnosis, k=21)

## Tuning KNN 
### Tune by adjust K value. K=10-15 appears to be a good value
ks <- c(1,5,10,12,15,17,20,25,30,40) # multiple k values
### target is the location of the predicted column, in wbcd it is diagnosis
errors <- sapply(ks, crossValidationError, features=wbcd_n, target=wbcd[,1])
plot(errors~ks, main="Cross Validation Error VsK",
     xlab="k",
     ylab="CVError")
lines(errors~ks)

## Z-Score Standardization
### Uses built in function scale() to re-scale values using the z-score
### standradization method. Scale() can be directly applied to data frames.
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

### test z-score with crossValidationError function made earlier. k=21 does not
### appear to be improved. K=5-10 appears to be a good value.
crossValidationError(wbcd_z, wbcd[,1], k=21)
errors <- sapply(ks, crossValidationError, features=wbcd_z, target=wbcd[,1])
plot(errors~ks, main="Cross Validation Error VsK",
     xlab="k",
     ylab="CVError")
lines(errors~ks)
