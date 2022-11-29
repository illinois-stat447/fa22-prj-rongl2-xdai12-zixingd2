# BoW for Drug Dataset

# prepare and load the data
library(SnowballC)
library(tm)
# setting the working directory where the text reviews dataset is located # recollect that we pre-processed and transformed the raw dataset format setwd('/home/sunil/Desktop/sentiment_analysis/')
# reading the transformed file as a dataframe
train_text <- read.table(file='Drug/Drug Train.csv', sep=',',header = TRUE)
test_text <- read.table(file='Drug/Drug Test.csv', sep=',',header = TRUE)
# checking the dataframe to confirm everything is in tact
print(dim(test_text))
dim(train_text)


# function to transform corpus and create dtm
text2dtm <- function(text) {
  # transforming the text into volatile corpus
  corp = VCorpus(VectorSource(text$SentimentText))
  print(corp)
  
  # creating document term matrix
  dtm <- DocumentTermMatrix(corp, 
                            control = list(tolower = TRUE, 
                                           removeNumbers = TRUE, 
                                           stopwords = TRUE, 
                                           removePunctuation = TRUE,
                                           stemming = TRUE))
  # Basic EDA on dtm
  inspect(dtm)
  
  # Removing sparse terms
  dtm <- removeSparseTerms(dtm, 0.99)
  inspect(dtm)
  dtm
}

# get the dtm dataset and labels
train <- text2dtm(train_text)
test <- text2dtm(test_text)

dtm_train_labels <- as.factor(as.character(train_text$Sentiment)) 
dtm_test_labels <- as.factor(as.character(test_text$Sentiment))

# convert the cell values with a non-zero value to Y, and in case of a zero
cellconvert<- function(x) {
  x <- ifelse(x > 0, "Y", "N")
}

# applying the function to rows in training and test datasets 
train <- apply(train, MARGIN = 2, cellconvert) 
test <- apply(test, MARGIN = 2, cellconvert)
# inspecting the train dtm to confirm all is in tact 
View(train)


# training the naive bayes classifier on the training dtm 
library(e1071) 
nb_senti_classifier=naiveBayes(train, dtm_train_labels) 
# printing the summary of the model created 
summary(nb_senti_classifier)

# making predictions on the test data dtm 
nb_predicts<-predict(nb_senti_classifier, test, type="class") 
# printing the predictions from the model
print(nb_predicts)


# computing accuracy of the model
library(rminer)
print(mmetric(nb_predicts, dtm_test_labels, c("ACC")))









