library(ggplot2)
library(forcats)
library(dplyr)
library(reshape2)
library(DMwR)
library(caret)
library(doParallel)
library(klaR)
library(gmodels)

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

kdd$num_compromised = NULL
kdd$serror_rate = NULL
kdd$rerror_rate = NULL
kdd$srv_serror_rate = NULL
kdd$srv_rerror_rate = NULL
kdd$srv_count = NULL
kdd$dst_host_srv_rerror_rate = NULL
kdd$dst_host_same_srv_rate = NULL
kdd$dst_host_srv_serror_rate = NULL

## Rebuild correlation matrix
corr_mat = melt(get_upper_matrix(cor(kdd[sapply(kdd,is.numeric)])))
ggplot(data = corr_mat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color="white") +
  scale_fill_gradient2(midpoint=0,limit=c(-1,1)) +
  theme(axis.text.x=element_text(angle=90, vjust=.5, hjust=1))

## protocol_type
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(protocol_type)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("protocol type") +
  ylab("count of protocol types") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

### service
as.data.frame(sort(table(kdd$service)))

### flag
as.data.frame(sort(table(kdd$flag)))
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(flag)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("flag type") +
  ylab("count of flags") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()


## eventType
as.data.frame(sort(table(kdd$eventType)))

ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(eventType)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("event type") +
  ylab("count of event types") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

## eventCategory
as.data.frame(sort(table(kdd$eventCategory)))

ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(eventCategory)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("event category") +
  ylab("count of event category") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

## src_bytes and dst_bytes
plot(table(kdd$src_bytes))

# Models
set.seed(1)
kdd$eventType = NULL

## Train and test data - standard sampling
train_index = createDataPartition(kdd$eventCategory, p=0.9, list=FALSE)
kdd_train = kdd[train_index,]

kdd_test = kdd[-train_index,-32]
kdd_test_y = kdd[-train_index,32]

# Under and Oversample
## 1000 gave ~.866 accuracy
## 10000 gave ~.907 accuracy
table(kdd_train$eventCategory)
kdd_smote = SMOTE(form=eventCategory~., data=kdd_train, k=5, perc.over=10000)
table(kdd_smote$eventCategory)

kdd_bind = rbind(kdd_train[kdd_train$eventCategory != "u2r",], kdd_smote[kdd_smote$eventCategory == "u2r",])
table(kdd_bind$eventCategory)

kdd_down = downSample(x=kdd_bind, y=kdd_bind$eventCategory, list=FALSE)
table(kdd_down$eventCategory)

kdd_down_x = kdd_down[,-32]
kdd_down_y = kdd_down[,32]

## Naive Bayes
cl = makePSOCKcluster(detectCores(logical=FALSE))
registerDoParallel(cl)
kdd_classifier <- naiveBayes(kdd_down_x, kdd_down_y)
test_pred <- predict(kdd_classifier, kdd_test)
stopCluster(cl)

CrossTable(test_pred, kdd_test_y, prop.chisq=FALSE, prop.t=FALSE,
           dnn=c("predicted","actual"))

## Alt Naive Bayes
kdd_nb = train(kdd_down_x, kdd_down_y, 
               method="nb",
               trControl=trainControl(method="cv",
                                      number=5,
                                      allowParallel=TRUE,
                                      savePredictions="all"))
predic_nb = predict(kdd_nb, as.matrix(kdd_test))
kdd_nb
## Decision Tree


## ANN