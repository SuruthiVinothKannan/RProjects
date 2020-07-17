
getwd()
setwd ("C:\\Users\\Suruthi\\Desktop\\Data\\Sentiment Analysis")

library(openxlsx)

data1 <- read.xlsx("input.xlsx")

# Packages tm(text mining) and SnowballC helps to clean the unstructured data like articles removal,
# Stem word removal, Preposition removal and special character removal

install.packages("tm")
install.packages("SnowballC")

# To do sentiment analysis we are installing RSentiment packages and wordcloud to visualise
# the final ouput.

install.packages("RSentiment")
install.packages("wordcloud")

library(tm)
library(SnowballC)
# library(ROauth)
library(RSentiment)
library(wordcloud)


str(data1)


# Making the unstructured text data as a array before creating a corpus
tweets <- array(data1$comment)

#?VectorSource
#docs <- c("This is a text.", "This another one.")
#(vs <- VectorSource(docs))
#inspect(VCorpus(vs))

# Corpus <- Keeping all information in one place. Here all the tweets will be cleaned at the same time.
txt_corpus<- Corpus (VectorSource (tweets))

# remove white space
tm_map<- tm_map (txt_corpus, stripWhitespace)
# remove punctuations
tm_map<- tm_map (tm_map, removePunctuation)
# to remove numbers
tm_map<- tm_map (tm_map, removeNumbers)
# to remove stop words(like 'as' 'the' etc..)
tm_map<- tm_map (tm_map, removeWords, stopwords("english"))
# remove URLs
removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)
tm_map<- tm_map(tm_map, removeURL)
#to create stem document
tm_map<- tm_map (tm_map, stemDocument)

#After cleaning we got the corpus with unique words.
#Next we need to create the frequency table for the unique words for this we have a function
#Termdocumentmatrix which will treat each word as a variable and will give us the frequency of occurence.

#creating termdocumentmatrix
Matrix <- TermDocumentMatrix(tm_map) 
# converting into matrix
matrix_c<- as.matrix (Matrix)
#exporting termdocument matrix
write.csv(matrix_c,"matrix.csv")
# frequency data
freq<- sort (rowSums (matrix_c))
#creating wordcloud
tmdata<- data.frame (words=names(freq), freq)
wordcloud (tmdata$words, tmdata$freq, max.words=100, min.freq=3, scale=c(7,.5), random.order=FALSE, colors=brewer.pal(8,"Dark2"))



# functionscore.sentiment
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  # create simple array of scores with laply
  require(plyr)
  require(stringr)
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches= !is.na(pos.matches)
                   neg.matches= !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

# import the files containing the positive and negative words
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")
data1 <- read.xlsx("matrix.xlsx")
pairs(data1)
# apply function score.sentiment
scores = score.sentiment(tweets, pos, neg, .progress='text')
#creating sentiment variable to existing data frame

scores$sentiment[scores$score>0] <- "Positive"
scores$sentiment[scores$score<0] <- "Negative"
scores$sentiment[scores$score==0] <- "Neutral"

write.csv(scores,"scores.csv")



#creating adataframe with positive tweets
positive=subset(scores,sentiment=='Positive')
#creating a dataframe with negative tweets
negative=subset(scores,sentiment=='Negative')
#word cloud for positive tweets
txt_corpus<- Corpus (VectorSource (positive$text))
# remove white space
tm_map<- tm_map (txt_corpus, stripWhitespace)
# remove punctuations
tm_map<- tm_map (tm_map, removePunctuation)
# to remove numbers
tm_map<- tm_map (tm_map, removeNumbers)
# to remove stop words(like 'as' 'the' etc..)
tm_map<- tm_map (tm_map, removeWords, stopwords("english"))
# remove URLs
removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)
tm_map<- tm_map(tm_map, removeURL) 
#to create stem document
tm_map<- tm_map (tm_map, stemDocument)
#Plain text document
# tm_map<-tm_map(tm_map,PlainTextDocument)
#creating termdocumentmatrix
Matrix <- TermDocumentMatrix(tm_map) 
# converting into matrix
matrix_c<- as.matrix (Matrix)
# frequency data
freq<- sort (rowSums (matrix_c))
#creating wordcloud
tmdata<- data.frame (words=names(freq), freq)
wordcloud (tmdata$words, tmdata$freq, max.words=100, min.freq=3, scale=c(7,.5), random.order=FALSE, colors=brewer.pal(8,"Dark2"))



#word cloud for negative tweets
txt_corpus<- Corpus (VectorSource (negative$text))
# remove white space
tm_map<- tm_map (txt_corpus, stripWhitespace)
# remove punctuations
tm_map<- tm_map (tm_map, removePunctuation)
# to remove numbers
tm_map<- tm_map (tm_map, removeNumbers)
# to remove stop words(like 'as' 'the' etc..)
tm_map<- tm_map (tm_map, removeWords, stopwords("english"))
# remove URLs
removeURL<- function(x) gsub("http[[:alnum:]]*", "", x)
tm_map<- tm_map(tm_map, removeURL) 
#to create stem document
tm_map<- tm_map (tm_map, stemDocument)
#Plain text document
# tm_map<-tm_map(tm_map,PlainTextDocument)
#creating termdocumentmatrix
Matrix <- TermDocumentMatrix(tm_map) 
# converting into matrix
matrix_c<- as.matrix (Matrix)
# frequency data
freq<- sort (rowSums (matrix_c))
#creating wordcloud
tmdata<- data.frame (words=names(freq), freq)
wordcloud (tmdata$words, tmdata$freq, max.words=100, min.freq=3, scale=c(7,.5), random.order=FALSE, colors=brewer.pal(8,"Dark2"))





