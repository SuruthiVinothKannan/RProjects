getwd()
setwd ("C:\\Users\\Suruthi\\Desktop\\Data\\Sentiment Analysis")

library(openxlsx)
library(tm)
library(SnowballC)
# library(ROauth)
library(RSentiment)
library(wordcloud)


# Reading the input data
data1 <- read.xlsx("input.xlsx")
str(data1)
names(data1)
tweets<-array(data1$comment)


# Building Corpus
corpus<-iconv(tweets, to="utf-8")
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:4])

# Cleaning text corpus
corpus<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:2])

corpus<-tm_map(corpus,removePunctuation)
inspect(corpus[1:2])

corpus<-tm_map(corpus,tolower)
inspect(corpus[1:2])

corpus<-tm_map(corpus,removeNumbers)
inspect(corpus[1:2])

corpus<-tm_map(corpus,removeWords,stopwords("english"))
inspect(corpus[1:2])

removeURL<- function(x) gsub("http[[:alnum:]]*","",x)
corpus<-tm_map(corpus,removeURL)
inspect(corpus[1:2])

removexdxd<- function(x) gsub("xdxd","",x,ignore.case = T)
cleanset<-tm_map(corpus,removexdxd)
inspect(cleanset[1:2])

# creating Term Document Matrix
tdm<- TermDocumentMatrix(cleanset)
tdm
tdm<-as.matrix(tdm)
tdm


# Bar plot
w<-(rowSums(tdm))
w<-subset(w,w>=20)
w
barplot(w,las=2,col = rainbow(50))

#Word Cloud
library(wordcloud)
w<-sort(rowSums(tdm),decreasing=TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 10,
          colors = brewer.pal(8,'Dark2'))

# Sentiment Analysis using Syuzhet
library(syuzhet)

s<-get_nrc_sentiment(tweets)
head(s)
senti<-colSums(s)

#Bar plot
barplot(senti,
        las=2,
        col=rainbow(16),
        ylab = 'Overall word Count',
        main = 'Sentiment Score')

# Sentiment Analysis using package SentimentAnalysis

library(SentimentAnalysis)

getwd()
data <- read.xlsx("input.xlsx")

twts<-array(data$comment)


# Building Corpus
doc<-iconv(twts, to="utf-8")
doc<-Corpus(VectorSource(doc))
inspect(doc[1:4])

# Cleaning text doc
doc<-tm_map(doc,stripWhitespace)
inspect(doc[1:2])

doc<-tm_map(doc,removePunctuation)
inspect(doc[1:2])

doc<-tm_map(doc,tolower)
inspect(doc[1:2])

doc<-tm_map(doc,removeNumbers)
inspect(doc[1:2])

doc<-tm_map(doc,removeWords,stopwords("english"))
inspect(doc[1:2])

removeURL<- function(x) gsub("http[[:alnum:]]*","",x)
doc<-tm_map(doc,removeURL)
inspect(doc[1:2])

removexdxd<- function(x) gsub("xdxd","",x,ignore.case = T)
doc<-tm_map(doc,removexdxd)
inspect(doc[1:2])

sentimentVal <- analyzeSentiment(doc)
names(sentimentVal)
sentiment<-convertToDirection(sentimentVal$SentimentQDAP)
head(sentiment)
data$sentiment<-sentiment

# data with the sentiment
data

plot(data$sentiment,
        las=2,
        col=rainbow(6),
        main = 'Sentiment Score')
