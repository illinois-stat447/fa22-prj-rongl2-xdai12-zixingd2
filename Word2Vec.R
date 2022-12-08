# including the required library 
# install.packages("https://cran.r-project.org/src/contrib/Archive/softmaxreg/softmaxreg_1.2.tar.gz",repos = NULL, type = "source")
library(softmaxreg) 
# importing the word2vec pretrained vector into memory 
data(word2vec)

# function to get word vector for each review 
docVectors = function(x) { 
  wordEmbed(x, word2vec, meanVec = TRUE) 
} 
# setting the working directory and reading the reviews dataset 
# setwd('/Users/Lorrie/Documents/UIUC/stat447 data science programming methods/final project/amazon_review_polarity_csv/')
setwd('/Users/Lorrie/Documents/UIUC/stat447 data science programming methods/final project/drugsCom_raw/')
# text = read.csv(file='Sentiment Analysis Dataset.csv', header = TRUE)
text = read.csv(file='Drug Train.csv', header = TRUE)
# applying the docVector function on each of the reviews 
# storing the matrix of word vectors as temp 
temp=t(sapply(text$SentimentText, docVectors)) 
# visualizing the word vectors output 
View(temp)
dim(temp)

#-------model training---------------------
# splitting the dataset into train and test 
cut = round(dim(temp)[1]*0.8,0)
temp_train=temp[1:cut,] 
temp_test=temp[(cut+1):dim(temp)[1],] 

labels_train=as.factor(as.character(text[1:cut,]$Sentiment)) 
labels_test=as.factor(as.character(text[(cut+1):dim(temp)[1],]$Sentiment)) 
# including the random forest library 
# install.packages("randomForest")
library(randomForest) 
# training a model using random forest classifier with training dataset 
# observe that we are using 20 trees to create the model 
rf_senti_classifier=randomForest(temp_train, labels_train,ntree=100) 
print(rf_senti_classifier)

#-------prediction-------------------------
# making predictions on the dataset 
rf_predicts<-predict(rf_senti_classifier, temp_test) 
# install.packages("rminer")
library(rminer) 
print(mmetric(rf_predicts, labels_test, c("ACC")))
