# Naive Bayes Classification

## Packages
library(tm)
library(SnowballC)
library(wordcloud)
library(e1071)
library(gmodels)
library(caret)

## Load Data
sms_raw <- read.csv("4_naive_bayes/data/sms_spam.csv", stringsAsFactors=FALSE)

sms_raw$type <- factor(sms_raw$type)

## Data Exploration
t <- table(sms_raw$type)
prop.table(t)

## Data Cleaning
### Create a collection of documents (emails/texts)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

### Inspect documents and show example content
inspect(sms_corpus[1:2])
as.character(sms_corpus[[5]])

### Display subset corpus elements, can use sapply or lapply
lapply(sms_corpus[5:6], as.character)

### Convert all text to lower case with tolower() function using tm_map. It is
### specifically designed to work with corpus from the tm package in R. Applies
### assigned function to every document within a corpus. Content_transformer 
### allows use of functions not in tm by default.
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
lapply(sms_corpus_clean[5:6], as.character)

### Remove numbers from corpus elements
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
lapply(sms_corpus_clean[5:6], as.character)

### Remove stop words (to, and, but, or) from the corpus elements using 
### stopwords() function. removeWords will be used as a vector of stop words.
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
lapply(sms_corpus_clean[5:6], as.character)

### Remove punctuations using function. replace all punctuation symbols with
### a blank space. gsub is for pattern matching and the text within the quotes
### is for all punctuation symbols.
replacePunctuation <- function(x) {
  gsub("[[:punct:]]+", " ", x)
}
sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           content_transformer(replacePunctuation))
lapply(sms_corpus_clean[5:6], as.character)

### Convert all words to the root word, dropping this like s/ing/ed
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
lapply(sms_corpus_clean[5:6], as.character)

### Remove white space
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
lapply(sms_corpus_clean[5:6], as.character)

### Split documents into document-term matrix (DTM)
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

## Create Training and Testing datasets
### sample size for test set
smp_size <- floor(.75 * nrow(sms_dtm))
smp_size_plus <- smp_size+1
pop_size <- as.numeric(nrow(sms_dtm))

### Training and Testing datasets via hold out method
sms_dtm_train <- sms_dtm[1:smp_size,]
sms_dtm_test <- sms_dtm[smp_size_plus:pop_size,]

### Training and Testing labels
sms_train_labels <- sms_raw[1:smp_size,]$type
sms_test_labels <- sms_raw[smp_size_plus:pop_size,]$type

### Validate the training and test sets have approximately close probabilities
prop.table(table(sms_dtm_train_labels))
prop.table(table(sms_dtm_test_labels))

## Visualizations
wordcloud(sms_corpus_clean, min.freq=50, random.order=FALSE)
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, .5))
wordcloud(ham$text, max.words = 40, scale = c(3, .5))

## Word Frequencies
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
sms_dtm_freq_train <- sms_dtm_train[ ,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

### Function to convert numeric column to yes or no categorical values if the 
### word appeared at all.
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

### Apply the convert_counts function over the data subsets' columns. MARGIN=2 
### is to apply function to columns only. Returned is a matrix where each column
### is a word and the value is Yes or No for if the word was present in the row.
sms_train <- apply(sms_dtm_freq_train, MARGIN=2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN=2, convert_counts)

## Training the model
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

## Evaluate Performance
### predict() is a function to make predictions, we are storing the value
sms_test_pred <- predict(sms_classifier, sms_test)

### CrossTab to evaluate actual versus predicted data. Both prop arguments 
### remove unnecessary proportions that show up in the CrossTab. dnn renames the
### rows and columns. The CrossTab shows 27 mistakes were made with 
### classification. This shows an error rate of 1.9%. 4 false positives and 23
### false negatives total. 
CrossTable(sms_test_pred, sms_test_labels, prop.chisq=FALSE, prop.t=FALSE,
           dnn=c("predicted","actual"))

## Improving model performance
### Laplace estimator included. The model error rate increased to 2.0% but there
### were 2 less false positives. Thus, if it was important to have the least 
### amount of false positives, even with the error rate increasing this would
### be a better outcome.
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace=1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq=FALSE, prop.t=FALSE,
           dnn=c("predicted","actual"))

## K-fold cross validation
### Interested in False Positive Rate (FPR) and False Negative Rate (FNR). Goal
### is to have a low FPR. Below is what 5 folds will look like.
folds <- createFolds(sms_raw$type, k=5)
str(folds)

### function to take in a fold (row number), document-term matrix, true labels,
### and laplacian correction value. returns the FPR and FNR.
naiveBayes_fold <- function(fold, features, target, laplace=0) {
  train = features[-fold,]
  validation = features[fold,]
  train_labels = target[-fold]
  validation_labels = target[fold]
  NaiveBayes_model = naiveBayes(train,train_labels,laplace=laplace)
  validation_preds = predict(NaiveBayes_model, validation)
  t = table(validation_labels,validation_preds)
  FPR = t[1,2]/(t[1,2]+t[1,1])
  FNR = t[2,1]/(t[2,1]+t[2,2])
  return (c("FPR"=FPR,"FNR"=FNR))
}

### Validation fold function that calls the naive Bayes fold function for each
### fold passed.
crossValidationError <- function(features, target, laplace=0, n_folds) {
  folds = createFolds(target,k=n_folds)
  errors = sapply(folds, naiveBayes_fold, features=features, target=target,
               laplace=laplace)
  return(rowMeans(errors))
}

### Call CVE function. A laplace of 1 was an increased error.
errors <- crossValidationError(features=rbind(sms_train, sms_test), 
                               target=sms_raw$type,
                               n_folds=5)
print(errors)