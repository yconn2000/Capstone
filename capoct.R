
library(DT)
library(stringr)
library(NLP)
library(tm)
library(RWeka)
library(DT)


setwd('C:/Users/Admin B/Documents/R/cap')

# Read in data
blogs <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul=TRUE)
news <- readLines("./final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul=TRUE)
twitter <- readLines("./final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul=TRUE)
bad_words <- readLines("./final/en_US/badwords.txt", encoding = "UTF-8", skipNul=TRUE)

# Sample 12K lines
sampleTwitter <- twitter[sample(1:length(twitter),12000)]
sampleNews <- news[sample(1:length(news),12000)]
sampleBlogs <- blogs[sample(1:length(blogs),12000)]
textSample <- c(sampleTwitter,sampleNews,sampleBlogs)


# Start data cleaning
clean_data <- Corpus(VectorSource(textSample))
clean_data <- tm_map(clean_data, content_transformer(function(x) 
                        iconv(x, to="UTF-8", sub="byte")), mc.cores=1)
clean_data <- tm_map(clean_data, content_transformer(tolower), lazy = TRUE)
clean_data <- tm_map(clean_data, content_transformer(removePunctuation))
clean_data <- tm_map(clean_data, content_transformer(removeNumbers))
clean_data <- tm_map(clean_data, content_transformer(removeURL))
clean_data <- tm_map(clean_data, stripWhitespace)
clean_data <- tm_map(clean_data, removeWords, stopwords("english"))
clean_data <- tm_map(clean_data, removeWords, bad_words)
clean_data <- tm_map(clean_data, stemDocument)
clean_data <- tm_map(clean_data, stripWhitespace)


saveRDS(clean_data, file = "./final_step.rds")
finalCorpus <- readRDS("./final_step.rds")
finalCorpusDF <-data.frame(text=unlist(sapply(finalCorpus,`[`, "content")), stringsAsFactors = FALSE)

# Tokenization the n-grams
ngramTokenizer <- function(theCorpus, ngramCount) {
  ngramFunction <- NGramTokenizer(theCorpus, Weka_control(min = ngramCount, max = ngramCount, 
                   delimiters = " \\r\\n\\t.,;:\"()?!"))
  ngramFunction <- data.frame(table(ngramFunction))
  ngramFunction <- ngramFunction[order(ngramFunction$Freq, decreasing = TRUE),][1:10,]
  colnames(ngramFunction) <- c("String","Count")
  ngramFunction
}

onegram <- ngramTokenizer(finalCorpusDF, 1)
saveRDS(onegram, file = "./data/1gram.rds")
twogram <- ngramTokenizer(finalCorpusDF, 2)
saveRDS(twogram, file = "./data/2gram.rds")
threegram <- ngramTokenizer(finalCorpusDF, 3)
saveRDS(threegram, file = "./data/3gram.rds")

