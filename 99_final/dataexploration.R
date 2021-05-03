library(caret)
library(doParallel)
library(dplyr)
library(forcats)
library(ggplot2)
library(gmodels)
library(pROC)
library(data.table)
library(keras)
library(mltools)
library(tfruns)
install_keras(method="conda", envname="r", tensorflow="gpu")
options(scipen=10000)

#Data Import
kdd_header = read.csv("99_final/data/header.csv",header=FALSE)
kdd_header = as.character(unlist(kdd_header[1,]))

kdd = read.csv("99_final/data/kddcup_10percent.csv", 
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
apply(kdd, 2, function(x) length(unique(x)))
kdd$num_outbound_cmds = NULL

## Filter nulls
colSums(is.na(kdd))
kdd = kdd[complete.cases(kdd),]
colSums(is.na(kdd))

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
get_upper_matrix <- function(matrix){
  matrix[lower.tri(matrix)] <- NA
  return(matrix)
}
corr_mat = melt(get_upper_matrix(cor(kdd[sapply(kdd,is.numeric)])))
ggplot(data = corr_mat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color="white") +
  scale_fill_gradient2(midpoint=0,limit=c(-1,1)) +
  theme(axis.text.x=element_text(angle=90, vjust=.5, hjust=1))

## Measure correlation to drop one
### Drop: num_compromised, serror_rate, rerror_rate, srv_serror_rate, 
### srv_rerror_rate, srv_count, dst_host_srv_rerror_rate, dst_host_same_srv_rate,
### dst_host_srv_serror_rate
corr_filt = corr_mat[abs(corr_mat$value) > .9 & 
                       abs(corr_mat$value) < 1 & 
                       !is.na(corr_mat$value),]
corr_filt[order(corr_filt$Var1),]

# Columns to drop
## Drop for correlation
kdd$num_compromised = NULL
kdd$serror_rate = NULL
kdd$rerror_rate = NULL
kdd$srv_serror_rate = NULL
kdd$srv_rerror_rate = NULL
kdd$srv_count = NULL
kdd$dst_host_srv_rerror_rate = NULL
kdd$dst_host_same_srv_rate = NULL
kdd$dst_host_srv_serror_rate = NULL
kdd$is_hot_login = NULL
## Drop to reduce class imbalance and use category instead
kdd$eventType = NULL
## Drop due to complexity in factor levels (70 levels)
kdd$service = NULL

# Categoric
## Rebuild correlation matrix
corr_mat = melt(get_upper_matrix(cor(kdd[sapply(kdd,is.numeric)])))
ggplot(data = corr_mat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color="white") +
  scale_fill_gradient2(midpoint=0,limit=c(-1,1)) +
  theme(axis.text.x=element_text(angle=90, vjust=.5, hjust=1))

## protocol_type
as.data.frame(sort(table(kdd$protocol_type)))
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(protocol_type)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("protocol type") +
  ylab("count of protocol types") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

### flag
as.data.frame(sort(table(kdd$flag)))
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(flag)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("flag type") +
  ylab("count of flags") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

## eventCategory
as.data.frame(sort(table(kdd$eventCategory)))
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(eventCategory)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("event type") +
  ylab("count of event types") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

## land
as.data.frame(sort(table(kdd$land)))
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(land)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("land type") +
  ylab("count of land type") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

## logged in
as.data.frame(sort(table(kdd$logged_in)))
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(logged_in)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("logged in status") +
  ylab("count of logged in status") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

## root shell
as.data.frame(sort(table(kdd$root_shell)))
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(root_shell)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("shell level") +
  ylab("count of shell level") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

## su attempt
as.data.frame(sort(table(kdd$su_attempted)))
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(su_attempted)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("su attempt") +
  ylab("count of su attempt") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

## guest login
as.data.frame(sort(table(kdd$is_guest_login)))
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(is_guest_login)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("guest login") +
  ylab("count of guest login") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

# Numeric
## Outliers excluded to aid visualizations
colfunc = colorRampPalette(c("orange2","orangered4"))

boxplot(duration~eventCategory, data=kdd,
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(src_bytes~eventCategory, data=kdd,
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(dst_bytes~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(count~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(dst_host_count~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(dst_host_srv_count~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(hot~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(num_file_creations~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(same_srv_rate~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(diff_srv_rate~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(dst_host_diff_srv_rate~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(dst_host_same_src_port_rate~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(dst_host_srv_diff_host_rate~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(dst_host_serror_rate~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

boxplot(dst_host_rerror_rate~eventCategory, data=kdd, 
        col=colfunc(length(levels(kdd$eventCategory))),
        outline=FALSE)

# Subsetting Data
set.seed(1)
train_index = createDataPartition(kdd$eventCategory, p=0.9, list=FALSE)
kdd_test_x = as.data.frame(kdd[-train_index,-30])
kdd_test_y = as.data.frame(kdd[-train_index, 30])
colnames(kdd_test_y)[1] = "eventCategory"
kdd_train = kdd[train_index,]

val_index = createDataPartition(kdd_train$eventCategory, p=0.1, list=FALSE)
kdd_val_x = as.data.frame(kdd_train[val_index,-30])
kdd_val_y = as.data.frame(kdd_train[val_index, 30])
colnames(kdd_val_y)[1] = "eventCategory"
kdd_train_x = kdd_train[-val_index,-30]
kdd_train_y = kdd_train[-val_index, 30]

# Naive Bayes
## TP of ~70%
## ~.92-93 accuracy with 5 fold cv
set.seed(1)
cl = makePSOCKcluster(detectCores(logical=FALSE))
registerDoParallel(cl)
kdd_nb = caret::train(kdd_train_x, kdd_train_y, 
                      method="nb",
                      trControl=trainControl(method="cv",
                                             number=8,
                                             sampling="down",
                                             allowParallel=TRUE,
                                             classProbs=TRUE,
                                             savePredictions=TRUE))
probs_nb = predict(kdd_nb, kdd_test_x, type="prob")
raw_nb = predict(kdd_nb, kdd_test_x, type="raw")
stopCluster(cl)

CrossTable(raw_nb, kdd_test_y$eventCategory, prop.chisq=FALSE, prop.t=FALSE,
           dnn=c("predicted","actual"))
auc(multiclass.roc(kdd_test_y$eventCategory, probs_nb))

# Random Forest
## TP ~97%
## Appears to net ~99% accuracy on test
set.seed(1)
cl = makePSOCKcluster(detectCores(logical=FALSE))
registerDoParallel(cl)
kdd_rf = caret::train(kdd_train_x,kdd_train_y,
                      method="rf",
                      trControl=trainControl(method="cv",
                                             number=10,
                                             sampling="down",
                                             allowParallel=TRUE,
                                             savePredictions=TRUE,
                                             classProbs=TRUE,
                                             verboseIter=TRUE),
                      ntree=15)
probs_rf = predict(kdd_rf, kdd_test_x, type="prob")
raw_rf = predict(kdd_rf, kdd_test_x, type="raw")
stopCluster(cl)

CrossTable(raw_rf, kdd_test_y$eventCategory, prop.chisq=FALSE, prop.t=FALSE,
           dnn=c("predicted","actual"))
auc(multiclass.roc(as.factor(kdd_test_y$eventCategory), probs_rf))
varImp(kdd_rf)

# ANN
## One Hot Encoding
set.seed(1)
kdd_train_y_df = as.data.frame(kdd_train_y)
colnames(kdd_train_y_df)[1] = "eventCategory"

kdd_train_y_ohe = as.data.frame(one_hot(as.data.table(kdd_train_y_df)))
kdd_train_x_ohe = as.data.frame(one_hot(as.data.table(kdd_train_x)))
kdd_val_y_ohe = as.data.frame(one_hot(as.data.table(kdd_val_y)))
kdd_val_x_ohe = as.data.frame(one_hot(as.data.table(kdd_val_x)))
kdd_test_y_ohe = as.data.frame(one_hot(as.data.table(kdd_test_y)))
kdd_test_x_ohe = as.data.frame(one_hot(as.data.table(kdd_test_x)))

## Train
## Original flags with 2% sample: 
### nodes=8,32,64;lr=.1,.01,.001;batch=32,50,100;epoch=50,100;activation=relu,sigmoid,tanh
## Best flag:
### node=64,batch=20,activation=relu,lr=.001,epoch=10
cl = makePSOCKcluster(detectCores(logical=FALSE))
registerDoParallel(cl)
runs <- tuning_run("99_final/kdd_hyper.R", 
                   flags = list(nodes=c(32),
                                learning_rate=c(0.001), 
                                batch_size=c(32),
                                epochs=c(100),
                                activation=c("relu"))
                   )
stopCluster(cl)

runs
view_run(runs$run_dir[which.max(runs$metric_val_accuracy)])
## Evaluate Model
### Model
model <- keras_model_sequential() %>%
  layer_dense(units=32, activation="relu", input_shape=dim(kdd_train_x_ohe)[2]) %>%
  layer_dense(units=32, activation="relu") %>% 
  layer_dense(units=5, activation='softmax') # 5 category classifiers
### Compile Model
model %>% compile(optimizer = optimizer_adam(lr=.001), 
                  loss = 'categorical_crossentropy',
                  metrics = c('accuracy'))
### Train Model
model %>% fit(as.matrix(kdd_train_x_ohe), as.matrix(kdd_train_y_ohe), 
              batch_size=32,
              epochs=100, 
              validation_data=list(as.matrix(kdd_val_x_ohe), as.matrix(kdd_val_y_ohe))
)
model %>% evaluate(as.matrix(kdd_test_x_ohe), as.matrix(kdd_test_y_ohe))
predictions = model %>% predict(as.matrix(kdd_test_x_ohe))

### Convert column names and object class to evaluate performance
colnames(predictions) = colnames(kdd_test_y_ohe)
predictions = as.data.frame(predictions)
kdd_test_y_ohe = as.data.frame(kdd_test_y_ohe)
predictions = predictions %>% rename("dos" = "eventCategory_dos",
                                     "normal" = "eventCategory_normal",
                                     "probe" = "eventCategory_probe",
                                     "r21" = "eventCategory_r21",
                                     "u2r" = "eventCategory_u2r")
kdd_test_y_ohe = kdd_test_y_ohe %>% rename("dos" = "eventCategory_dos",
                                           "normal" = "eventCategory_normal",
                                           "probe" = "eventCategory_probe",
                                           "r21" = "eventCategory_r21",
                                           "u2r" = "eventCategory_u2r")
### Generate CrossTable and AUC
w <- which(predictions==apply(predictions[,], 1, max), arr.ind = TRUE)
predictions_ct <- toupper(names(round(predictions))[w[order(w[,1]),2]])
w2 <- which(kdd_test_y_ohe==apply(kdd_test_y_ohe[,], 1, max), arr.ind = TRUE)
kdd_test_y_ct <- toupper(names(kdd_test_y_ohe)[w2[order(w2[,1]),2]])

CrossTable(predictions_ct, kdd_test_y_ct, prop.chisq=FALSE, prop.t=FALSE,
           dnn=c("predicted","actual"))

auc(multiclass.roc(kdd_test_y$eventCategory, predictions))
