### Fast-text
# prepare
# reading the first 1000 reviews from the dataset 
reviews_text<-readLines('amazon_review_polarity_csv/train.csv', n = 1000)
# basic EDA to confirm that the data is read correctly 
print("before")
print(class(reviews_text))
print(length(reviews_text))
print(head(reviews_text,2))
# replacing the positive sentiment value 2 with __label__2 
reviews_text<-gsub("\\\"2\\\",","__label__2 ",reviews_text)
# replacing the negative sentiment value 1 with __label__1 
reviews_text<-gsub("\\\"1\\\",","__label__1 ",reviews_text)
# removing the unnecessary \" characters
reviews_text<-gsub("\\\""," ",reviews_text)
# replacing multiple spaces in the text with single space 
reviews_text<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", reviews_text, perl=TRUE)
# Basic EDA post the required processing to confirm input is as desired 
print("after")
print(class(reviews_text))
print(length(reviews_text))
print(head(reviews_text,2))
# writing the revamped file to the directory so we could use it with fastText sentiment analyzer project 
fileConn<-file("Dataset/Sentiment Analysis Dataset_ft.txt")
writeLines(reviews_text, fileConn)
close(fileConn)

library(fastTextR)
ftext = readLines("Dataset/Sentiment Analysis Dataset_ft.txt")
# dividing the reviews into training and test
temp_train=ftext[1:800]
temp_test=ftext[801:1000]
# Viewing the train datasets for confirmation
# View(temp_train)
# View(temp_test)


# creating txt file for train and test dataset
# the fasttext function expects files to be passed for training and testing 
fileConn<-file("Dataset/train_ft.txt") 
writeLines(temp_train, fileConn)
close(fileConn) 
fileConn<-file("Dataset/test_ft.txt") 
writeLines(temp_test, fileConn)
close(fileConn)
# creating a test file with no labels
# recollect the original test dataset has labels in it as the dataset is just a subset obtained from full dataset 
temp_test_nolabel<- gsub("__label__1", "", temp_test, perl=TRUE) 
temp_test_nolabel<- gsub("__label__2", "", temp_test_nolabel, perl=TRUE)

fileConn<-file("Dataset/test_nolabel_ft.txt") 
writeLines(temp_test_nolabel, fileConn)
close(fileConn)
# training a supervised classification model with training dataset file
# ctrl = ft_control(learning_rate = 0.1,
#                   word_vec_size = 10L,
#                   epoch = 5L,
#                   min_count = 1L,
#                   max_len_ngram = 2L,
#                   nbuckets = 10000000L,
#                   nthreads = 4L,
#                   seed = 0L)
ctrl <- ft_control(learning_rate = 1.9,
                   word_vec_size = 10L,
                   epoch = 5L,
                   min_count = 1L,
                   max_len_ngram = 2L,
                   nbuckets = 10000000L,
                   nthreads = 2L,
                   seed = 0L)
model<-ft_train("Dataset/train_ft.txt", method = "supervised", control = ctrl)
# Obtain all the words from a previously trained model=
# words<-ft_words(model)
# # viewing the words for confirmation. These are the set of words present in our training data
# View(words)
# 
# 
# word_vec<-ft_word_vectors(model, words)
# View(word_vec)

# predicting the labels for the reviews in the no labels test dataset
# and writing it to a file for future reference
# getting the predictions into a dataframe so as to compute performance
test = readLines("Dataset/test_nolabel_ft.txt")
ft_preds <- ft_predict(model, newdata= test)

# reading the test file to extract the actual labels
reviewstestfile <- readLines("Dataset/test_ft.txt")
# extracting just the labels frm each line
library(stringi)
actlabels<-stri_extract_first(reviewstestfile, regex="\\w+")
# converting the actual labels and predicted labels into factors 
actlabels<-as.factor(as.character(actlabels)) 
ft_preds_labels <-as.factor(as.character(ft_preds$label))
# getting the estimate of the accuracy
library(rminer)
print(mmetric(actlabels, ft_preds, c("ACC")))
ft_test(model, "Dataset/train_ft.txt")
ft_test(model, "Dataset/test_ft.txt")

        
        
        
        