#From Jockers
#load libraries
library(mallet)
library(wordcloud)

#load the corpus
inputDir <- "/Users/msaxton/r_projects/iliff_review_project/iliff_corpus"
files.v <- dir(path=inputDir, pattern=".*txt")

#this function divides the texts into chunks for processing
makeFlexTextChunks <- function(doc.object, chunk.size=1000, percentage=TRUE){
  words.lower <- tolower(doc.object)
  words.lower <- gsub("[^[:alnum:][:space:]']", " ", words.lower)
  words.l <- strsplit(words.lower, "\\s+")
  word.v <- unlist(words.l)
  x <- seq_along(word.v)
  if(percentage){
    max.length <- length(word.v)/chunk.size
    chunks.l <- split(word.v, ceiling(x/max.length))
  } else {
    chunks.l <- split(word.v, ceiling(x/chunk.size))
    #deal with small chunks at the end
    if(length(chunks.l[[length(chunks.l)]]) <=
       length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l)-1]] <-
        c(chunks.l[[length(chunks.l)-1]],
          chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]] <- NULL
    }
  }
  chunks.l <- lapply(chunks.l, paste, collapse=" ")
  chunks.df <- do.call(rbind, chunks.l)
  return(chunks.df)
}

#the following creates a matrix 2294 x 2 where the rows represent sections of texts
#and the columns are segment name, text
topic.m <- NULL
for(i in 1:length(files.v)){
  doc.object <- scan(file.path(inputDir,files.v[i]), what="character")
  chunk.m <- makeFlexTextChunks(doc.object, chunk.size=1000, percentage = FALSE)
  textname <- gsub("\\..*","", files.v[i])
  segments.m <-cbind(paste(textname, segment=1:nrow(chunk.m), sep="_"), chunk.m)
  topic.m <- rbind(topic.m, segments.m)
}

#convert the matrix to data frame
documents <- as.data.frame(topic.m, stringsAsFactors = FALSE)
colnames(documents) <- c("id", "text")

# invoke mallet.import function
mallet.instances <- mallet.import(documents$id, documents$text, "stoplist.csv", FALSE, 
                                  token.regexp = "[\\p{L}']+")

#create a topic model trainer object
topic.model <- MalletLDA(num.topics=10)

topic.model$loadDocuments(mallet.instances)
topic.model$train(400)

#explore the topic model
topic.words.m <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)

vocabulary <- topic.model$getVocabulary()
colnames(topic.words.m) <- vocabulary

# 13.7 Topic Visualization
topic.top.words <- mallet.top.words(topic.model, topic.words.m[imp.row,], 100)

wordcloud(topic.top.words$words, topic.top.words$weights, c(4, .8), rot.per=0, random.order = FALSE)

#13.8 topic cohernence and topic probability
doc.topics.m <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
#
file.ids.v <- documents[,1]
file.id.l <- strsplit(fil)