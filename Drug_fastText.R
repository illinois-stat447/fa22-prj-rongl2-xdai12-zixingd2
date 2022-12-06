### Fast-text for Drug Dataset
library(tidyr)
library(dplyr)
# load and prepare the data
train_text <- readLines("Drug/Drug Train.csv")
head(train_text)
test_text <- readLines("Drug/Drug Test.csv")

# function to do basic EDA, label conversion for FastText input, and return the new dataset
labelFinalizer <- function(data) {
  # basic EDA to confirm that the data is read correctly 
  print("before")
  print(class(data))
  print(length(data))
  print(head(data,2))
  # replacing the positive sentiment value 2 with __label__2 
  data <- gsub("\\\"2\\\",","__label__2 ", data)
  # replacing the negative sentiment value 1 with __label__1 
  data <- gsub("\\\"1\\\",","__label__1 ", data)
  # removing the unnecessary \" characters
  data <- gsub("\\\""," ", data)
  # replacing multiple spaces in the text with single space 
  data <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", data, perl=TRUE)
  # remove header
  data <- data[-1]
  # Basic EDA post the required processing to confirm input is as desired 
  print("after")
  print(class(data))
  print(length(data))
  print(head(data,2))
  # return
  data
}

train_ft <- labelFinalizer(train_text)
test_ft <- labelFinalizer(test_text)


# model Training and Prediction
library(fastTextR)
# creating txt file for train and test dataset
# the fasttext function expects files to be passed for training and testing 
fileConn <- file("Drug/train_ft.txt") 
writeLines(train_ft, fileConn)
close(fileConn) 
fileConn <- file("Drug/test_ft.txt") 
writeLines(test_ft, fileConn)
close(fileConn)
# creating a test file with no labels
# recollect the original test dataset has labels in it as the dataset is just a subset obtained from full dataset 
temp_test_nolabel <- gsub("__label__1", "", test_ft, perl=TRUE) 
temp_test_nolabel <- gsub("__label__2", "", temp_test_nolabel, perl=TRUE)

fileConn <- file("Drug/test_nolabel_ft.txt") 
writeLines(temp_test_nolabel, fileConn)
close(fileConn)

# training a supervised classification model with training dataset file 
ctrl <- ft_control(learning_rate = 0.15,
                   word_vec_size = 10L,
                   epoch = 5L,
                   min_count = 1L,
                   max_len_ngram = 2L,
                   nbuckets = 10000000L,
                   nthreads = 5L,
                   seed = 0L)


# ctrl <- ft_control(nthreads = 5L, seed = 0L)
model <- ft_train("Drug/train_ft.txt", 
                  method = "supervised", 
                  control = ctrl)
# Obtain all the words from a previously trained model=
words <- ft_words(model)
# viewing the words for confirmation. These are the set of words present in our training data
# View(words)

word_vec <- ft_word_vectors(model, words)
# View(word_vec)

# predicting the labels for the reviews in the no labels test dataset
# and writing it to a file for future reference
# getting the predictions into a dataframe so as to compute performance
test <- readLines("Drug/test_nolabel_ft.txt")
ft_preds <- ft_predict(model, newdata = test)

# evaluate the model
ft_test(model, "Drug/train_ft.txt")
ft_test(model, "Drug/test_ft.txt")
