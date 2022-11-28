library(tidyr)
library(dplyr)
setwd('/Users/Lorrie/Documents/UIUC/stat447 data science programming methods/final project/drugsCom_raw/')

# data = read.csv(file='drugsComTrain_raw.tsv', sep='\t',header = TRUE,encoding = 'utf-8') 
data = read.csv(file='drugsComTest_raw.tsv', sep='\t',header = TRUE,encoding = 'utf-8') 

data$review <- gsub("[^[:alnum:] ]","",data$review)
data$review <- gsub("039","'",data$review)
data$review <- gsub("I Ve","I've",data$review)
data$review <- gsub("I've","I have",data$review)
data$review <- gsub("I'm","I am",data$review)
data <- unite(data, col='SentimentText', c('drugName', 'condition', 'review'), sep=' ')
data$SentimentText <- gsub("[^[:alnum:] ]","",data$SentimentText)
# Replacing multiple spaces in the text with single space 
data$SentimentText<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", data$SentimentText, perl=TRUE)
# remove leading or trailing space
data$SentimentText<-trimws(data$SentimentText)

rating_map <- function(x){
  if (x <= 4){
    1
  }else if(x >=7){
    2
  }else{
    "#"
  }
}
data$rating <- sapply(data$rating, rating_map)

final_data <- data |> 
  rename('Sentiment'='rating') |> 
  filter(Sentiment != "#") |>
  select('Sentiment','SentimentText')

print(dim(final_data))

# write.table(final_data,file = "Drug Train.csv",row.names = F,col.names = T,sep=',')

write.table(final_data,file = "Drug Test.csv",row.names = F,col.names = T,sep=',')
