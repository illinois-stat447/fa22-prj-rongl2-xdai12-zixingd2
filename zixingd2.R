# read data for two dataset (full&polarity)
review = read.csv("amazon_review_full_csv/train.csv", header = FALSE)
# head(review)

pol = read.csv("amazon_review_polarity_csv/train.csv", header = FALSE)
# head(pol)


# As the replication from the book, we use polarity dataset first
# but instead of using read.txt, we use read.scv

# merge title & body column, and create new dataset with new columns
pol$text = paste(pol$V2, pol$V3)
colnames(pol)[1] = "Sentiment"
plrty = pol[1:1000, c(1, 4)]

# processing the data
library(SnowballC)
library(tm)
# create corpus
train_corp = VCorpus(VectorSource(plrty$text))
print(train_corp)
# creating document term matrix
dtm_train <- DocumentTermMatrix(train_corp, 
                                control = list(tolower = TRUE, 
                                               removeNumbers = TRUE,
                                               stopwords = TRUE,
                                               removePunctuation = TRUE,
                                               stemming = TRUE)
                                )
# Basic EDA on dtm
inspect(dtm_train)













