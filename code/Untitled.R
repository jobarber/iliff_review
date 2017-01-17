'''
modified code from
https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
'''
library(tm)
library(topicmodels)

setwd("/Users/msaxton/r_projects/iliff_review_project/iliff_corpus")
filenames <- list.files(getwd(), pattern="*.txt")

#read files into a character vecor
files <- lapply(filenames, readLines)

#create corpus from vector
docs <- Corpus(VectorSource(files))

#preprocessing
#lower case
docs <- tm_map(docs, content_transformer(tolower))
#remove symbols
toSpace <- content_transformer(function(x, pattern) 
  { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, '"')

#more preprocessing
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords('en'))
docs <- tm_map(docs, stripWhitespace)

#Stem document (or should it be lemmatized?)
docs <- tm_map(docs, stemDocument)

#create document-term matrix
dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- filenames

#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))

#create sort order (descending)
ord <- order(freq, decreasing = TRUE)

#list all terms in decreasing order of freq and write to disk
word.freq <- freq[ord]
#write.csv(x, "iliff_word_freq.csv")

#set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003, 5, 63, 100001, 765)
nstart <- 5
best <- TRUE

#set number of topics
k <- 10

#run LDA using Gibbs sampling
ldaOut <- LDA(dtm, k, method = "Gibbs", control = list(
  nstar=nstart, seed=seed, best=best, burnin = burnin,
  iter = iter, thin = thin))


# docs to topics; [i.e. document to topic assignments]
ldaOut.topics <- as.matrix(topics(ldaOut))

#top ten term in each topic
ldaOut.terms <- as.matrix(terms(ldaOut, 10))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)

#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

#write output to csv files
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))