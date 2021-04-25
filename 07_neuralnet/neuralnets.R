# Regression Artifical Neural Networks

## Packages
library(dplyr)
library(ggplot2)
library(tensorflow)
#devtools::install_github("rstudio/keras")
library(keras)
library(reticulate)
install_keras(method="conda", envname="r", tensorflow="gpu")

## Import Data
concrete <- read.csv("07_neuralnet/data/concrete.csv")

## Explore Data
str(concrete)

## Train and Test Dataset
concrete_train <- concrete[1:773,-9]
train_labels <- concrete[1:773,9]
concrete_test <- concrete[774:1030,-9]
test_labels <- concrete[774:1030,9]

## Normalize
### Z-score normalization using training data only
concrete_train <- scale(concrete_train)
col_means_train <- attr(concrete_train, "scaled:center")
col_stddevs_train <- attr(concrete_train, "scaled:scale")

### Apply training normalization to test data
concrete_test <- scale(concrete_test, 
                       center=col_means_train, 
                       scale=col_stddevs_train)

## Modeling
### One hidden layer with 5 neurons, Relu activation function for hidden layer.
### No activation function for last layer. %>% operation forwards output to
### input of next command. layer_dense is a feedforward network
model <- keras_model_sequential() %>%
  layer_dense(units=8, activation = "relu",
              input_shape= dim(concrete_train)[2]) %>%
  layer_dense(units=1)

### Specify loss function to be Mean Squared Error and 
### Stoachistc Gradient Descent for optimizer
model %>% compile(loss="mse",
                  optimizer="sgd")

## set seed
set.seed(1)

## Train Model
history <- model %>% fit(concrete_train,
                         train_labels,
                         batch_size=50,
                         epochs=125,
                         validation_data=list(concrete_test, test_labels))

## Evaluate Model
predict_labels <- model %>% predict(concrete_test)

rmse <- function(x,y) {
  return((mean((x-y)^2))^.5)
}

rmse(predict_labels,test_labels)
summary(test_labels)
sd(test_labels)
### Error was ~6.8, much less than 1 Standard Deviation of ~16.3

# Classification Artifical Neural Networks

## Packages
library(keras)

## Import Data - already split in train/test sets
fashion_mnist <- dataset_fashion_mnist()
class_names= c('Tshirt/top','Trouser','Pullover','Dress','Coat', 
               'Sandal','Shirt','Sneaker','Bag','Ankleboot')

## Explore Data
str(fashion_mnist)

### Visualize several images
par(mfrow=c(5,5))
par(mar=c(0,0,1.5,0))
for (i in 1:25) {
  img <- fashion_mnist$train$x[i, , ]
  img <- t(apply(img, 2, rev))
  image(1:28, 1:28, img, col=gray(seq(0, 1, length=255)),
        xaxt='n', yaxt='n', 
        main=paste(class_names[fashion_mnist$train$y[i] + 1])
        ) 
}

## Train and Test Data
fashion_train <- fashion_mnist$train
fashion_train$x <- fashion_train$x/255 # divide by 255 to normalize
fashion_test <- fashion_mnist$test
fashion_test$x <- fashion_test$x/255 # divide by 255 to normalize
### Validation Set
fashion_val <- list()
fashion_val$x <- fashion_train$x[50001:60000, ,]
fashion_val$y <- fashion_train$y[50001:60000]
fashion_train$x <- fashion_train$x[1:50000, ,]
fashion_train$y <- fashion_train$y[1:50000]

## Model
### 1 hidden layer
model <- keras_model_sequential() %>%
  layer_flatten(input_shape=c(28,28)) %>% # flatten 28x28 pixel images
  layer_dense(units=128, activation='relu') %>% # 128 neurons
  layer_dense(units=10, activation='softmax') # 10 category classifiers
### Compile model
model %>% compile(
  optimizer='adam',
  loss='sparse_categorical_crossentropy', # not One Hot Encoded so use sparse
  metrics=c('accuracy'))

## set seed
set.seed(111)

## Train Model
model %>% fit(fashion_train$x, 
              fashion_train$y,
              batch_size=100,
              epochs=30,
              validation_data=list(fashion_val$x, fashion_val$y)
              )

## Evaluate Model
model %>% evaluate(fashion_test$x, fashion_test$y)

# Hyper Parameter Tuning

## Packages
library(tfruns)

## Modeling
runs <- tuning_run("7_neuralnet/fashion_mnist.R", 
                   flags = list(nodes=c(64, 128, 392),
                                learning_rate=c(0.01, 0.05, 0.001, 0.0001), 
                                batch_size=c(100,200,500,1000),
                                epochs=c(30,50,100),
                                activation=c("relu","sigmoid","tanh")),
                   sample = 0.02 # Take a 2% sample of the entire 432 combinations
                   )

## Evaluate model
view_run(runs$run_dir[5]) # Change number to whichever is highest accuracy

# Retrain highest accuracy model
## Modeling
model <- keras_model_sequential() %>%
  layer_flatten(input_shape=c(28,28)) %>% # flatten 28x28 pixel images
  layer_dense(units=392, activation='relu') %>% # 128 neurons
  layer_dense(units=10, activation='softmax') # 10 category classifiers
### Compile model
model %>% compile(
  optimizer='adam',
  loss='sparse_categorical_crossentropy', # not One Hot Encoded so use sparse
  metrics=c('accuracy'))

## set seed
set.seed(111)

## Train Model
model %>% fit(fashion_train$x, 
              fashion_train$y,
              batch_size=200,
              epochs=50,
              validation_data=list(fashion_val$x, fashion_val$y)
)
## Evaluate Model
model %>% evaluate(fashion_test$x, fashion_test$y)
predictions <- model %>% predict(fashion_test$x)

## Examine predictions
predictions[1,] # predictions for first image in data
which.max(predictions[1,])-1 # subtract 1 because labels start at 0
class_pred <- model %>% predict_classes(fashion_test$x)

## Plot predictions
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), 
    xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- fashion_test$x[i, , ]
  img <- t(apply(img, 2, rev)) 
  predicted_label <- class_pred[i]
  true_label<-fashion_test$y[i]
  if (predicted_label== true_label) {
    color <-'blue' 
  } 
  else {
    color <-'red'
    }
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt= 'n', yaxt= 'n',
        main = paste(class_names[predicted_label+ 1], " (",
                     class_names[true_label+ 1], ")"),
        col.main= color)
  }