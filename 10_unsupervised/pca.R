# PCA example using ISLR

## Packages
library(ISLR)
library(caret)
library(e1071)

## Data Exploration using USArrests 
pr.out = prcomp(USArrests, scale=TRUE)

names(pr.out)

pr.out$rotation
pr.out$x
biplot(pr.out, scale=0)
pr.out$sdev
pr.var = pr.out$sdev^2
PVE = pr.var/sum(pr.var)
cumsum(PVE)

## Example PCA Breast cancer data
set.seed(1)
wisc = read.csv("10_unsupervised/data/wisc_bc_data.csv", stringsAsFactor=TRUE)
wisc$id = NULL

in_train = createDataPartition(wisc$diagnosis, p=.8, list=FALSE)

bc_train = wisc[in_train,]
bc_test = wisc[-in_train,]

### Basic KNN using pre-existing variables
knn_model = train(diagnosis~., data=bc_train,
                  trControl=trainControl("cv", number=10),
                  preProcess=c("center","scale"),
                  method="knn")
knn_model

predictions_knn = predict(knn_model, bc_test)
confusionMatrix(predictions_knn, bc_test$diagnosis)

### KNN using PCA
knn_model_pca = train(diagnosis~., data=bc_train,
                      trControl=trainControl("cv",number=10),                
                        preProcess=c("center","scale","pca"),                
                        method="knn")
knn_model_pca
knn_model_pca$preProcess

predictions_knn_pca = predict(knn_model_pca, bc_test)
confusionMatrix(predictions_knn_pca, bc_test$diagnosis)
