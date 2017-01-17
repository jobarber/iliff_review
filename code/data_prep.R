# load necessary libraries
library(tm)
library(koRpus)

# load custom stopword list
ir_stopwords <- read.csv("data/ir_stoplist.csv", header = FALSE)
ir_stopwords <- as.character(ir_stopwords$V1)

#functions
abstractLemmas <- function(file){
  tagged.text <- treetag(file, treetagger = "manual",
                         lang="en", TT.options=list(path="/Users/msaxton/treetagger/", 
                                                    preset="en"))
  results <- taggedText(tagged.text)
  lemmas <- results[,"lemma"]
  return(lemmas)
}

makeCorpus <- function(filenames){
  file_path <- paste("data/iliff_corpus/", filenames, sep="")
  files <- lapply(file_path, abstractLemmas)
  text_corpus <- Corpus(VectorSource(files))
  return(text_corpus)
}

preprocessCorpus <- function(text_corpus){
  text_corpus <- tm_map(text_corpus, stripWhitespace)
  text_corpus <- tm_map(text_corpus, content_transformer(tolower))
  text_corpus <- tm_map(text_corpus, removeWords,ir_stopwords )
  text_corpus <- tm_map(text_corpus, removeNumbers)
  text_corpus <- tm_map(text_corpus, removePunctuation)
  return(text_corpus)
}

createDTM <- function(corpus, filenames){
  dtm <- DocumentTermMatrix(corpus)
  rownames(dtm) <- filenames
  return(dtm)
}

createCSV <- function(dtm, file_name){
  m <- inspect(dtm)
  DF <- as.data.frame(m, stringsAsFactors = FALSE)
  write.csv(DF, file = file_name)
}
# load and divide corpus by decade

filenames60s <- list.files("./iliff_corpus", pattern="^5|^6")
filenames70s <- list.files("./iliff_corpus", pattern="^7")
filenames80s <- list.files("./iliff_corpus", pattern="^8")

corpus60s <- makeCorpus(filenames60s)
corpus60s <- preprocessCorpus(corpus60s)
dtm60s <- createDTM(corpus60s, filenames60s)
createCSV(dtm60s, "data/dtm/dtm60s.csv")

corpus70s <- makeCorpus(filenames70s)
corpus70s <- preprocessCorpus(corpus70s)
dtm70s <- createDTM(corpus70s, filenames70s)
createCSV(dtm70s, "data/dtm/dtm70s.csv")

corpus80s <- makeCorpus(filenames80s)
corpus80s <- preprocessCorpus(corpus80s)
dtm80s <- createDTM(corpus80s, filenames80s)
createCSV(dtm80s, "data/dtm/dtm80s.csv")
