# exactly what the book did
reviews_text<-readLines('amazon_review_polarity_csv/train.csv', n = 1000)
# converting the reviews_text character vector to a dataframe 
reviews_text<-data.frame(reviews_text)
# # visualizing the dataframe
# View(reviews_text)

# separating the sentiment and the review text
# post separation the first column will have the first 4 characters
# second column will have the rest of the characters
# first column should be named "Sentiment"
# second column to be named "SentimentText"
library(tidyr)
reviews_text<-separate(data = reviews_text, col = reviews_text, into = c("Sentiment", "SentimentText"), sep = 4)
# viewing the dataset post the column split
View(reviews_text)

# Retaining only alphanumeric values in the sentiment column 
reviews_text$Sentiment<-gsub("[^[:alnum:] ]","",reviews_text$Sentiment) 
# Retaining only alphanumeric values in the sentiment text 
reviews_text$SentimentText<-gsub("[^[:alnum:] ]"," ",reviews_text$SentimentText)
# Replacing multiple spaces in the text with single space 
reviews_text$SentimentText<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", 
                                 reviews_text$SentimentText, perl=TRUE)
# Viewing the dataset
View(reviews_text)
# Writing the output to a file that can be consumed in other projects 
write.table(reviews_text,file = "Dataset/Sentiment Analysis Dataset.csv",row.names = F,col.names = T,sep=',')

library(SnowballC)
library(tm)
# setting the working directory where the text reviews dataset is located # recollect that we pre-processed and transformed the raw dataset format setwd('/home/sunil/Desktop/sentiment_analysis/')
# reading the transformed file as a dataframe
text <- read.table(file='Dataset/Sentiment Analysis Dataset.csv', sep=',',header = TRUE)
# checking the dataframe to confirm everything is in tact
print(dim(text))

# transforming the text into volatile corpus
train_corp = VCorpus(VectorSource(text$SentimentText))
print(train_corp)

# creating document term matrix
dtm_train <- DocumentTermMatrix(train_corp, control = list(
  tolower = TRUE,removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))
# Basic EDA on dtm
inspect(dtm_train)

# Removing sparse terms
dtm_train= removeSparseTerms(dtm_train, 0.99)
inspect(dtm_train)

# splitting the train and test DTM
dtm_train_train <- dtm_train[1:800, ]
dtm_train_test <- dtm_train[801:1000, ]
dtm_train_train_labels <- as.factor(as.character(text[1:800, ]$Sentiment)) 
dtm_train_test_labels <- as.factor(as.character(text[801:1000, ]$Sentiment))

# convert the cell values with a non-zero value to Y, and in case of a zero
cellconvert<- function(x) {
  x <- ifelse(x > 0, "Y", "N")
}

# applying the function to rows in training and test datasets 
dtm_train_train <- apply(dtm_train_train, MARGIN = 2,cellconvert) 
dtm_train_test <- apply(dtm_train_test, MARGIN = 2,cellconvert)
# inspecting the train dtm to confirm all is in tact 
View(dtm_train_train)


# training the naive bayes classifier on the training dtm 
library(e1071) 
nb_senti_classifier=naiveBayes(dtm_train_train,dtm_train_train_labels) 
# printing the summary of the model created 
summary(nb_senti_classifier)

# making predictions on the test data dtm 
nb_predicts<-predict(nb_senti_classifier, dtm_train_test,type="class") 
# printing the predictions from the model
print(nb_predicts)


# computing accuracy of the model
library(rminer)
print(mmetric(nb_predicts, dtm_train_test_labels, c("ACC")))









