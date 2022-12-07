# data = read.csv("amazon_review_polarity_csv/train.csv",header = FALSE)
# dim(data)


# read data and visualize the records
reviews_text<-readLines("amazon_review_polarity_csv/train.csv", n = 1000)
reviews_text<-data.frame(reviews_text)
View(reviews_text)

# modify the format
library(tidyr)
reviews_text<-separate(data = reviews_text, col = reviews_text, into = c("Sentiment", "SentimentText"), sep = 4)
View(reviews_text)

# Retaining only alphanumeric values in the sentiment column 
reviews_text$Sentiment<-gsub("[^[:alnum:] ]","",reviews_text$Sentiment)
# Retaining only alphanumeric values in the sentiment text 
reviews_text$SentimentText<-gsub("[^[:alnum:] ]"," ",reviews_text$SentimentText)
# Replacing multiple spaces in the text with single space 
reviews_text$SentimentText<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", reviews_text$SentimentText, perl=TRUE)
# Writing the output to a file that can be consumed in other projects 
write.table(reviews_text,file = "amazon_review_polarity_csv/Sentiment Analysis Dataset.csv",row.names = F,col.names = T,sep=',')


# data preprocessing for fastText algorithm
# reading the first 1000 reviews from the dataset 
reviews_text2<-readLines('amazon_review_polarity_csv/train.csv', n = 1000) 
# basic EDA to confirm that the data is read correctly 
print(class(reviews_text2)) 
print(length(reviews_text2)) 
print(head(reviews_text2,2)) 
# replacing the positive sentiment value 2 with __label__2 
reviews_text2<-gsub("\\\"2\\\",","__label__2 ",reviews_text2) 
# replacing the negative sentiment value 1 with __label__1 
reviews_text2<-gsub("\\\"1\\\",","__label__1 ",reviews_text2) 
# removing the unnecessary \" characters 
reviews_text2<-gsub("\\\""," ",reviews_text2) 
# replacing multiple spaces in the text with single space 
reviews_text2<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", reviews_text2, perl=TRUE) 
# Basic EDA post the required processing to confirm input is as desired 
print("EDA POST PROCESSING") 
print(class(reviews_text2)) 
print(length(reviews_text2)) 
print(head(reviews_text2,2)) 
# writing the revamped file to the directory so we could use it with # fastText sentiment analyzer project 
fileConn<-file("amazon_review_polarity_csv/Sentiment Analysis Dataset_ft.txt") 
writeLines(reviews_text2, fileConn) 
close(fileConn)
