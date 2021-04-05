library(tm)
library(dplyr)
library(quanteda)
library(wordcloud)
library(ngram)
library(ggplot2)
library(gridExtra)

###########  Getting and Cleaning Data ##########

# identifying the folder where the text are exist
folder <- "C:\\Users\\suhai\\Desktop\\All Folders\\R Project\\Smart-Keyboard-Data-Sciense-Course-Capstone-SwiftKey-\\en_US"

# find files within the folder
files <-list.files(path=folder, full.names = TRUE, recursive = TRUE)

# read first 10000 lines of all files and transform them into one corpus
corpus <- files %>% lapply(readLines, n = 10000) %>% lapply(paste, collapse = " ")

# reading profanity list
profanity <- readLines("list.txt")

# cleaning the corpus
corpus.clean <- corpus %>% gsub(pattern = "\\W", replace = " ") %>%
                gsub(pattern = "\\d", replace = " ") %>% tolower() %>%
                removeWords(profanity) %>% gsub(pattern = "\\b[A-z]\\b[1]", replace = " ") %>% 
                stripWhitespace()

# all the text in one string
alltext <- paste(corpus.clean[1], corpus.clean[2], corpus.clean[3])

# making a tm corpus
corpus.tm <- Corpus(VectorSource(alltext))


########## Exploratory Analysis ##########
# term document matrix creation 
tdm <- DocumentTermMatrix(corpus.tm)
freq <- as.data.frame(as.matrix(tdm))
ord <- order(freq,decreasing = TRUE) 
words.freq <- freq[ord]


# word cloud creation


# n-gram
unigram <- ngram(alltext, n=1)
uni.phrase.table <- get.phrasetable(unigram)

bigram <- ngram(alltext, n=2)
bi.phrase.table <- get.phrasetable(bigram)

trigram <- ngram(alltext, n=3)
tri.phrase.table <- get.phrasetable(trigram)


