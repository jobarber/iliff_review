"""
This is a short introduction to basic functionality of R based
on an article in Code4Lib by Monica Maceli which can be found at
http://journal.code4lib.org/articles/11626
"""

# Set up files and corpus
setwd("~/r_projects/")
inputDir <- "iliff_corpus"
files.v <- dir(path = inputDir, pattern = ".*txt")

# install necessary packages
install.packages("tm")
library(tm)
source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
install.packages("wordcloud")
library("wordcloud")

#load corpus
text_corpus <- Corpus(DirSource("~/r_projects/iliff_corpus"))

#clean corpus
text_corpus <- tm_map(text_corpus, stripWhitespace)
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))
text_corpus <- tm_map(text_corpus, stemDocument)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removePunctuation)

#create document term matrix
dtm <- DocumentTermMatrix(text_corpus)

#reduce dtm by adjusting sparsity
dtm2 <- removeSparseTerms(dtm, sparse=0.95)

#or
dtm3 <- DocumentTermMatrix(text_corpus, control = list(wordLengths=c(4, 20),
                                                       bounds =list(global =
                                                                      c(5,45))))
#finding term frequencies
findFreqTerms(dtm3, lowfreq = 200)
freq <- sort(colSums(as.matrix(dtm3)), decreasing = TRUE)
head(freq, 50)
tail(freq, 50)

#Finding associated terms
findAssocs(dtm3, term = "faith", 0.7)

#plot correlations
plot(dtm3, terms = names(findAssocs(dtm3,terms = "faith", 0.5)[["faith"]]),
     corThreshold = 0.5)

#wordclouds
wordcloud(names(freq), freq, min.freq=200)

#clustering: Notice we are creating a different kind of matrix
tdm <- TermDocumentMatrix(text_corpus)
tdm2 <- removeSparseTerms(tdm, sparse=0.1)
d <- dist(as.matrix(tdm2))
hc <- hclust(d)
plot(hc)

topTerms <- function(dtm){
  # freq <- findFreqTerms(dtm) #not sure what this line does
  sortfreq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
  return(sortfreq)
}

compareAssocTerms <- function(dtm_1, dtm_2, dtm_3, term, threshold=0.6){
  print(findAssocs(dtm_1, term, threshold))
  print(findAssocs(dtm_2, term, threshold))
  print(findAssocs(dtm_3, term, threshold))
}