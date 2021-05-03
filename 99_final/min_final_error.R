library(ggplot2)
library(forcats)
library(dplyr)
library(reshape2)
library(DMwR)
library(caret)
library(doParallel)
library(gmodels)
library(pROC)
library(keras)
library(tensorflow)
library(tfruns)
library(dummies)
install_keras(method="conda", envname="r", tensorflow="gpu")

options(scipen=10000)

#Data Import
kdd_header = read.csv("99_final/data/header.csv",header=FALSE)
kdd_header = as.character(unlist(kdd_header[1,]))

kdd = read.csv("99_final/data/kddcup.csv", 
               header=FALSE,
               stringsAsFactors=TRUE,
               col.names=kdd_header)

# Basic Cleaning
## Convert Binary and all discrete features
kdd$land = as.factor(kdd$land)
levels(kdd$land) = list("other"=0, "same"=1)

kdd$logged_in = as.factor(kdd$logged_in)
levels(kdd$logged_in) = list("fail"=0, "success"=1)

kdd$root_shell = as.factor(kdd$root_shell)
levels(kdd$root_shell) = list("nonroot"=0, "root"=1)

kdd$su_attempted = as.factor(kdd$su_attempted)
levels(kdd$su_attempted) = list("no"=0, "yes"=1)

kdd$is_hot_login = as.factor(kdd$is_hot_login)
levels(kdd$is_hot_login) = list("no"=0, "yes"=1)

kdd$is_guest_login = as.factor(kdd$is_guest_login)
levels(kdd$is_guest_login) = list("no"=0, "yes"=1)

## Clean event column values
levels(kdd$eventType) = gsub("\\.", "", levels(kdd$eventType))

## Filter columns of same value, one column
kdd$num_outbound_cmds = NULL

## Filter nulls
kdd = kdd[complete.cases(kdd),]

# Data Exploration of dependent variable
kdd = kdd %>% mutate(eventCategory = case_when(eventType %in% c("back","land","neptune",
                                                                "pod","smurf","teardrop") ~ "dos",
                                               eventType %in% c("buffer_overflow","loadmodule",
                                                                "perl","rootkit") ~ "u2r",
                                               eventType %in% c("ftp_write","guess_passwd","imap",
                                                                "multihop","phf","spy","warezclient",
                                                                "warezmaster") ~ "r21",
                                               eventType %in% c("ipsweep","nmap","portsweep",
                                                                "satan") ~ "probe",
                                               TRUE ~ "normal"))
kdd$eventCategory = as.factor(kdd$eventCategory)

# Data Exploration
## Correlation Matrix
## Measure correlation to drop one
### Drop: num_compromised, serror_rate, rerror_rate, srv_serror_rate, 
### srv_rerror_rate, srv_count, dst_host_srv_rerror_rate, dst_host_same_srv_rate,
### dst_host_srv_serror_rate

kdd$num_compromised = NULL
kdd$serror_rate = NULL
kdd$rerror_rate = NULL
kdd$srv_serror_rate = NULL
kdd$srv_rerror_rate = NULL
kdd$srv_count = NULL
kdd$dst_host_srv_rerror_rate = NULL
kdd$dst_host_same_srv_rate = NULL
kdd$dst_host_srv_serror_rate = NULL

# Models
set.seed(1)
## New column will make under and oversampling easier, won't impact a real-world
## IDS scenario
kdd$eventType = NULL
## Must remove due to 70 classes in factor adding complexity to modeling
kdd$service = NULL

## Train and test data - standard sampling
train_index = createDataPartition(kdd$eventCategory, p=0.9, list=FALSE)
kdd_train = kdd[train_index,]

kdd_test_x = kdd[-train_index,-31]
kdd_test_y = kdd[-train_index,31]

# Under and Oversample
table(kdd_train$eventCategory)
kdd_smote = SMOTE(form=eventCategory~., data=kdd_train, k=5, perc.over=10000)

kdd_bind = rbind(kdd_train[kdd_train$eventCategory != "u2r",], kdd_smote[kdd_smote$eventCategory == "u2r",])

kdd_down = downSample(x=kdd_bind[,-31], y=kdd_bind$eventCategory, list=FALSE, yname="eventCategory")

## ANN downsample validation set
val_index = createDataPartition(kdd_down$eventCategory, p=0.1, list=FALSE)
kdd_train_x = kdd_down[-val_index,-31]
kdd_train_y = as.data.frame(kdd_down[-val_index,31])
colnames(kdd_train_y)[1] = "eventCategory"
kdd_val_x = kdd_down[val_index,-31]
kdd_val_y = as.data.frame(kdd_down[val_index,31])
colnames(kdd_val_y)[1] = "eventCategory"

# ANN
set.seed(1)
### Modeling
kdd_train_y_ohe = as.data.frame(dummy.data.frame(kdd_train_y, sep="_"))
kdd_train_x_ohe = dummy.data.frame(kdd_train_x, sep="_")
kdd_val_y_ohe = dummy.data.frame(kdd_val_y, sep="_")
kdd_val_x_ohe = dummy.data.frame(kdd_val_x, sep="_")
missing = setdiff(names(kdd_train_x_ohe), names(kdd_val_x_ohe))
kdd_val_x_ohe[missing] = 0

# Parameters for dataset
FLAGS <-flags(flag_numeric("nodes", 30),
              flag_numeric("batch_size", 100),
              flag_string("activation", "relu"),
              flag_numeric("learning_rate", 0.01),
              flag_numeric("epochs", 30)
)

# Model
model <- keras_model_sequential() %>%
  layer_dense(units=FLAGS$nodes, activation=FLAGS$activation, input_shape=44) %>%
  layer_dense(units=FLAGS$nodes, activation=FLAGS$activation) %>% 
  layer_dense(units=5, activation='softmax') # 5 category classifiers

# Compile Model
model %>% compile(optimizer = optimizer_adam(lr=FLAGS$learning_rate), 
                  loss = 'categorical_crossentropy',
                  metrics = c('accuracy'))

# Train Model
model %>% fit(kdd_train_x_ohe, 
              kdd_train_y_ohe, 
              batch_size=FLAGS$batch_size,
              epochs=FLAGS$epochs, 
              validation_data=list(kdd_val_x_ohe,kdd_val_y_ohe)
)

runs
view_run(runs$run_dir[which.max(runs$metric_val_accuracy)])
### Evaluate Model
model %>% evaluate(kdd_test, kdd_test_y)