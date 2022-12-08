# Load the ggplot2 and wordcloud packages
install.packages(c('tm','wordcloud','ggplot2',"Rtsne"))
# install.packages("https://cran.r-project.org/src/contrib/Archive/softmaxreg/softmaxreg_1.2.tar.gz",repos = NULL, type = "source")
library(ggplot2)
library(wordcloud)
library(tm)
# including the required library 
library(softmaxreg) 
library(Rtsne)
# importing the word2vec pretrained vector into memory 
data(word2vec)

setwd('/Users/Lorrie/Documents/UIUC/stat447 data science programming methods/final project/drugsCom_raw/')
preprocessed_data <- read.csv("Drug Train.csv")
positive <- subset(preprocessed_data,Sentiment==2)
negative <- subset(preprocessed_data,Sentiment==1)

#------------------wordcloud--------------------------------------------
plotWordcloud <- function(preprocessed_data){
  drugCorpus <- VCorpus(VectorSource(preprocessed_data$SentimentText))
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  drugCorpus <- tm_map(drugCorpus, toSpace, "#")
  
  drugCorpus <- tm_map(drugCorpus, PlainTextDocument)
  drugCorpus <- tm_map(drugCorpus, removeNumbers)
  drugCorpus <- tm_map(drugCorpus, removeWords, c(stopwords("english"), 
                                                    "just", "like", "get", 
                                                    "time","month","year",
                                                  "day","week","take"))
  drugCorpus <- tm_map(drugCorpus, removePunctuation)
  drugCorpus <- tm_map(drugCorpus, stripWhitespace)
  drugCorpus <- tm_map(drugCorpus, stemDocument)
  
  tdm <- TermDocumentMatrix(drugCorpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing=TRUE)
  d <- data.frame(word = names(v), freq = v)
  
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=50, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"),scale=c(3,0.5))
}
par(mfrow=c(1,2),cex=3)
plotWordcloud(negative[1:5000,])
plotWordcloud(positive[1:5000,])
# basic EDA
# plot the length of sentences in positive and negative cases
# ---------------word length distribution-----------------------------------------
wordLenDist <- function(preprocessed){
  ggplot(preprocessed, aes(x=as.character(Sentiment), y=nchar(as.character(SentimentText))/1000, fill=Sentiment)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0,1)) +
    labs(title="Boxplot of Sentence Length by Sentiment", 
         x="Emotion", y = "Number of Characters in the Sentence (*1000)") +
    theme(legend.position = "none")
}
wordLenDist(preprocessed_data)

#----------------------t-SNE-------------------------------------------
data = as.array(temp)# the sentence embedding after applying 'docVectors' function
# data = data[!duplicated(data), ]
# data = data[1:2000,]
# tsne_output <- Rtsne(data,pca=TRUE,d=2,perplexity=30,check_duplicates = FALSE)
visSentenceEmbed <- function(data_embedding,data_origin){
  data = data[1:2000,]
  tsne_output <- Rtsne(data,pca=TRUE,d=2,perplexity=25,check_duplicates = FALSE)
  data = as.data.frame(data)
  ggplot(data,aes(x=tsne_output$Y[,1],y=tsne_output$Y[,2]))+
    geom_point(alpha=0.7,aes(colour=as.factor(data_origin$Sentiment[1:2000])))+#
    labs(title="Different sentiment class after applying t-SNE",color="Pos/Neg")+
    xlab("Feature1")+
    ylab("Feature2")
  
}
visSentenceEmbed(data,text)



