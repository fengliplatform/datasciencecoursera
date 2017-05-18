# set working dir
setwd("~/course/ds_coursera/capstone")

# Download data files
file.url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

if(!file.exists("./Coursera-SwiftKey.zip")) {
    download.file(file.url, destfile="./Coursera-SwiftKey.zip", method="curl")
}

if(!file.exists("./final")) {
    unzip("./Coursera-SwiftKey.zip")
}

# Read to R and save to RData
if(!file.exists("./blogs.RData")) {
    blogs.con <- file("./final/en_US/en_US.blogs.txt", "rb")
    blogs <- readLines(blogs.con, encoding="UTF-8", skipNul = TRUE, warn = FALSE) 
    close(blogs.con)
    save(blogs, file="blogs.RData")
}

if(!file.exists("./news.RData")) {
    news.con <- file("./final/en_US/en_US.news.txt", "rb")
    news <- readLines(news.con, encoding="UTF-8", skipNul = TRUE, warn = FALSE) 
    close(news.con)
    save(news, file="news.RData")
}

if(!file.exists("./twitter.RData")) {
    twitter.con <- file("./final/en_US/en_US.twitter.txt", "rb")
    twitter <- readLines(twitter.con, encoding="UTF-8", skipNul = TRUE, warn = FALSE) 
    close(twitter.con)
    save(twitter, file="twitter.RData")
}

# Load if necessary
load("blogs.RData")
load("news.RData")
load("twitter.RData")

# Sampling
# set random number to do sampling - only sample some data to do analysis.
blogs.nrow <- length(blogs)
news.nrow <- length(news)
twitter.nrow <- length(twitter)

set.seed(1234)
blogs.random <- as.logical(rbinom(n=blogs.nrow, size=1, prob=0.01))
set.seed(2345)
news.random <- as.logical(rbinom(n=news.nrow, size=1, prob=0.01))
set.seed(3456)
twitter.random <- as.logical(rbinom(n=twitter.nrow, size=1, prob=0.01))

# do sampling and create subsets
blogs.sample <- blogs[blogs.random]
news.sample <- news[news.random]
twitter.sample <- twitter[twitter.random]

save(blogs.sample, file="blogs.sample.RData")
save(news.sample, file="news.sample.RData")
save(twitter.sample, file="twitter.sample.RData")


# Load sampled data if necessary
load("blogs.sample.RData")
load("news.sample.RData")
load("twitter.sample.RData")

# Generate corpus
full.sample <- c(blogs.sample, news.sample, twitter.sample)

library(tm)

full.sample.corpus <- VectorSource(full.sample)
full.sample.corpus <- VCorpus(full.sample.corpus)
full.sample.corpus <- tm_map(full.sample.corpus, content_transformer(tolower))
full.sample.corpus <- tm_map(full.sample.corpus, removeNumbers)
full.sample.corpus <- tm_map(full.sample.corpus, removePunctuation)
full.sample.corpus <- tm_map(full.sample.corpus, stripWhitespace)
full.sample.corpus <- tm_map(full.sample.corpus, removeWords, stopwords(kind="en"))

save(full.sample.corpus, file="full.sample.corpus.RData")

# Tokenization
load("full.sample.corpus.RData")

library(RWeka)

full.sample.dtm1 <- DocumentTermMatrix(full.sample.corpus)
full.sample.dtm1.new <- removeSparseTerms(full.sample.dtm1,sparse = 0.99)
full.sample.dtm1.m <- as.matrix(full.sample.dtm1.new)
full.sample.frequency1 <- colSums(full.sample.dtm1.m)
full.sample.frequency1.sort <- sort(full.sample.frequency1, decreasing = TRUE)
save(full.sample.frequency1.sort, file="full.sample.frequency1.sort.RData")

ngram.tokenizer.2 <- function (x) NGramTokenizer(x, Weka_control(min=2, max=2))
full.sample.dtm2 <- DocumentTermMatrix(full.sample.corpus, control=list(tokenize=ngram.tokenizer.2))
full.sample.dtm2.new <- removeSparseTerms(full.sample.dtm2,sparse = 0.999)
full.sample.dtm2.m <- as.matrix(full.sample.dtm2.new)
full.sample.frequency2 <- colSums(full.sample.dtm2.m)
full.sample.frequency2.sort <- sort(full.sample.frequency2, decreasing = TRUE)
save(full.sample.frequency2.sort, file="full.sample.frequency2.sort.RData")
head(full.sample.frequency2.sort)

ngram.tokenizer.3 <- function (x) NGramTokenizer(x, Weka_control(min=3, max=3))
full.sample.dtm3 <- DocumentTermMatrix(full.sample.corpus, control=list(tokenize=ngram.tokenizer.3))
dim(full.sample.dtm3)
full.sample.dtm3.new <- removeSparseTerms(full.sample.dtm3,sparse = 0.9999)
dim(full.sample.dtm3.new)
full.sample.dtm3.m <- as.matrix(full.sample.dtm3.new)
full.sample.frequency3 <- colSums(full.sample.dtm3.m)
full.sample.frequency3.sort <- sort(full.sample.frequency3, decreasing = TRUE)
save(full.sample.frequency3.sort, file="full.sample.frequency3.sort.RData")
head(full.sample.frequency3.sort)

# generate data frames for n-grams data
load("full.sample.frequency1.sort.RData")
load("full.sample.frequency2.sort.RData")
load("full.sample.frequency3.sort.RData")

unigram.words <- names(full.sample.frequency1.sort)
save(unigram.words, file="unigram.words.RData")

bigram.words <- names(full.sample.frequency2.sort)
bigram.list <- strsplit(bigram.words, split=" ")
bigram.first <- unlist(lapply(bigram.list, `[[`, 1))
bigram.second <- unlist(lapply(bigram.list, `[[`, 2))
bigram.df <- data.frame(X=bigram.first, Y=bigram.second, stringsAsFactors=FALSE)
saveRDS(bigram.df, file = "bigram.RDS")

trigram.words <- names(full.sample.frequency3.sort)
trigram.list <- strsplit(trigram.words, split=" ")
trigram.first <- unlist(lapply(trigram.list, `[[`, 1))
trigram.second <- unlist(lapply(trigram.list, `[[`, 2))
trigram.third <- unlist(lapply(trigram.list, `[[`, 3))
# paste first and second
trigram.first.second <- paste(trigram.first, trigram.second, sep=" ")
trigram.df <- data.frame(X=trigram.first.second, Y=trigram.third, stringsAsFactors=FALSE)
saveRDS(trigram.df, file = "trigram.RDS")




