library(beepr)
# library(ngram)
# library(RWeka)
library(data.table)
# library(doSNOW)
library(tau)
library(plyr)
#library(parallel)

rm(list = ls())

if (Sys.info()['sysname'] == "Windows"){
      setwd("W:\\asc90698583\\Wuala Sync\\Diverses\\Coursera\\DATA SCIENCE SPECIALIZATION\\10 Capstone\\R\\")
} else setwd("/home/khl4v/Wuala Sync/Diverses/Coursera/DATA SCIENCE SPECIALIZATION/10 Capstone/R/")

source("makeNgrams.R")

en_Twitter <- readLines("Data\\en_US\\en_US.twitter.txt", encoding = "UTF-8")
en_Twitter <- readLines("Data/en_US/en_US.twitter.txt", encoding = "UTF-8")
en_News <- readLines("Data\\en_US\\en_US.news.txt", encoding = "UTF-8")
en_News <- readLines("Data/en_US/en_US.news.txt", encoding = "UTF-8")
en_Blogs <- readLines("Data\\en_US\\en_US.blogs.txt", encoding = "UTF-8")
en_Blogs <- readLines("Data/en_US/en_US.blogs.txt", encoding = "UTF-8")


# Filter profanity ----------------------------------------------------------
badwords <- readLines("Data/badwords_en.txt")
badwords <- c(badwords, "fucking")

en_Twitter <- readLines("Data/en_US/en_US.twitter.txt", encoding = "UTF-8")
badwordIndexTwitter <- sapply(en_Twitter, function(text){
      any(sapply(X = badwords, function(x) grepl(x, text)))
})
save(badwordIndexTwitter, file = "badwordIndexTwitter.RData")
badwordIndexTwitter <- as.logical(badwordIndexTwitter)
rm(en_Twitter)

en_News <- readLines("Data/en_US/en_US.news.txt", encoding = "UTF-8")
badwordIndexNews <- sapply(en_News, function(text){
      any(sapply(X = badwords, function(x) grepl(x, text)))
})
save(badwordIndexNews, file = "badwordIndexNews.RData")
badwordIndexNews <- as.logical(badwordIndexNews)
rm(en_News)

en_Blogs <- readLines("Data/en_US/en_US.blogs.txt", encoding = "UTF-8")
badwordIndexBlogs <- sapply(en_Blogs, function(text){
      any(sapply(X = badwords, function(x) grepl(x, text)))
})
save(badwordIndexBlogs, file = "badwordIndexBlogs.RData")
badwordIndexBlogs <- as.logical(badwordIndexBlogs)
rm(en_Blogs)

# Make n grams and skip n grams --------------------------------------------
# Twitter
load("badwordIndexTwitter.RData")
en_Twitter_clean <- en_Twitter[!badwordIndexTwitter]
# seed vergessen aber gespeichert
twitterTrainIndices <- sample(seq_along(en_Twitter_clean),
                              size = round(0.6 * length(en_Twitter_clean)),
                              replace = F)
tokensTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                            ngram = 1,
                            markSentences = F)
tokensTwitter <- tokensTwitter[count > 1]
bigramsTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                             ngram = 2,
                             markSentences = F)
bigramsTwitter <- bigramsTwitter[count > 1]
trigramsTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                              ngram = 3,
                              markSentences = F)
save(trigramsTwitter, file = "trigramsTwitter_clean.RData")
rm(trigramsTwitter)
gc()
fourgramsTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                              ngram = 4,
                              markSentences = F)
save(fourgramsTwitter, file = "fourgramsTwitter_clean.RData")
rm(fourgramsTwitter, en_Twitter_clean)
gc()

# News
load("badwordIndexNews.RData")
en_News <- readLines("Data/en_US/en_US.news.txt", encoding = "UTF-8")
en_News_clean <- en_News[!badwordIndexNews]
rm(en_News)
gc()

set.seed(1234)
newsTrainIndices <- sample(seq_along(en_News_clean),
                              size = round(0.6 * length(en_News_clean)),
                              replace = F)
tokensNews <- makeNgrams(en_News_clean[newsTrainIndices],
                         ngram = 1,
                         markSentences = F)
save(tokensNews, file = "tokensNews_clean")
rm(tokensNews)
gc()
bigramsNews <- makeNgrams(en_News_clean[newsTrainIndices],
                          ngram = 2,
                          markSentences = F)
save(bigramsNews, file = "bigramsNews_clean")
rm(bigramsNews)
gc()
trigramsNews <- makeNgrams(en_News_clean[newsTrainIndices],
                           ngram = 3,
                           markSentences = F)
save(trigramsNews, file = "trigramsNews_clean")
rm(trigramsNews)
gc()
fourgramsNews <- makeNgrams(en_News_clean[newsTrainIndices],
                           ngram = 4,
                           markSentences = F)
save(fourgramsNews, file = "fourgramsNews_clean")
rm(fourgramsNews, en_News_clean)
gc()


# Blogs
load("badwordIndexBlogs.RData")
en_Blogs <- readLines("Data/en_US/en_US.blogs.txt", encoding = "UTF-8")
en_Blogs_clean <- en_Blogs[!badwordIndexBlogs]
rm(en_Blogs)
gc()

set.seed(1234)
blogsTrainIndices <- sample(seq_along(en_Blogs_clean),
                           size = round(0.6 * length(en_Blogs_clean)),
                           replace = F)
tokensBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                         ngram = 1,
                         markSentences = F)
save(tokensBlogs, file = "tokensBlogs_clean")
rm(tokensBlogs)
gc()
bigramsBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                          ngram = 2,
                          markSentences = F)
save(bigramsBlogs, file = "bigramsBlogs_clean")
rm(bigramsBlogs)
gc()
trigramsBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                           ngram = 3,
                           markSentences = F)
save(trigramsBlogs, file = "trigramsBlogs_clean")
rm(trigramsBlogs)
gc()
fourgramsBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                            ngram = 4,
                            markSentences = F)
save(fourgramsBlogs, file = "fourgramsBlogs_clean")
rm(fourgramsBlogs)
gc()


# Skip-n-grams -------------------------------------------------------
# Twitter
load("badwordIndexTwitter.RData")
en_Twitter <- readLines("Data/en_US/en_US.twitter.txt", encoding = "UTF-8")
en_Twitter_clean <- en_Twitter[!badwordIndexTwitter]
rm(en_Twitter)
gc()
load("twitterTrainIndices.RData")
skipFiveGramsTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                                   skip = T,
                            ngram = 5,
                            markSentences = F)
save(skipFiveGramsTwitter, file = "skipFiveGramsTwitter_clean.RData")
rm(skipFiveGramsTwitter)
gc()
skipSixGramsTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                                  skip = T,
                             ngram = 6,
                             markSentences = F)
save(skipSixGramsTwitter, file = "skipSixGramsTwitter_clean.RData")
rm(skipSixGramsTwitter)
gc()

# News
load("badwordIndexNews.RData")
en_News <- readLines("Data/en_US/en_US.news.txt", encoding = "UTF-8")
en_News_clean <- en_News[!badwordIndexNews]
rm(en_News)
gc()
skipFiveGramsNews <- makeNgrams(en_News_clean[newsTrainIndices],
                                   skip = T,
                            ngram = 5,
                            markSentences = F)
save(skipFiveGramsNews, file = "skipFiveGramsNews_clean.RData")
rm(skipFiveGramsNews)
gc()
skipSixGramsNews <- makeNgrams(en_News_clean[newsTrainIndices],
                                  skip = T,
                             ngram = 6,
                             markSentences = F)
save(skipSixGramsNews, file = "skipSixGramsNews_clean.RData")
rm(skipSixGramsNews)
gc()

# Blogs
load("badwordIndexBlogs.RData")
badwordIndexBlogs <- as.logical(badwordIndexBlogs)
en_Blogs <- readLines("Data/en_US/en_US.blogs.txt", encoding = "UTF-8")
en_Blogs_clean <- en_Blogs[!badwordIndexBlogs]
rm(en_Blogs)
gc()
set.seed(1234)
blogsTrainIndices <- sample(seq_along(en_Blogs_clean),
                           size = round(0.6 * length(en_Blogs_clean)),
                           replace = F)
skipFiveGramsBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                                   skip = T,
                            ngram = 5,
                            markSentences = F)
save(skipFiveGramsBlogs, file = "skipFiveGramsBlogs_clean.RData")
rm(skipFiveGramsBlogs)
gc()
skipSixGramsBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                                  skip = T,
                             ngram = 6,
                             markSentences = F)
save(skipSixGramsBlogs, file = "skipSixGramsBlogs_clean.RData")
rm(skipSixGramsBlogs)
gc()


# Combine n-grams of the different sources ------------------------------------
load("tokensBlogs_clean.RData")
load("tokensNews_clean.RData")
load("tokensTwitter_clean.RData")
allTokens <- rbind.fill(tokensBlogs, tokensNews, tokensTwitter)
allTokens <- data.table(allTokens)
allTokens <- allTokens[, lapply(.SD, sum), by = ngram]
save(allTokens, file = "allTokens_clean.RData")
rm(tokensBlogs, tokensTwitter, tokensNews)

load("bigramsBlogs_clean.RData")
load("bigramsNews_clean.RData")
load("bigramsTwitter_clean.RData")
allBigrams <- rbind.fill(bigramsBlogs, bigramsNews, bigramsTwitter)
allBigrams <- data.table(allBigrams)
allBigrams <- allBigrams[, lapply(.SD, sum), by = ngram]
save(allBigrams, file = "allBigrams_clean.RData")
rm(bigramsBlogs, bigramsTwitter, bigramsNews)

load("trigramsBlogs_clean.RData")
load("trigramsNews_clean.RData")
load("trigramsTwitter_clean.RData")
allTrigrams <- rbind.fill(trigramsBlogs, trigramsNews, trigramsTwitter)
allTrigrams <- data.table(allTrigrams)
allTrigrams <- allTrigrams[, lapply(.SD, sum), by = ngram]
save(allTrigrams, file = "allTrigrams_clean.RData")
rm(trigramsBlogs, trigramsTwitter, trigramsNews)

load("fourgramsBlogs_clean.RData")
load("fourgramsNews_clean.RData")
load("fourgramsTwitter_clean.RData")
allFourgrams <- rbind.fill(fourgramsBlogs, fourgramsNews, fourgramsTwitter)
rm(fourgramsBlogs, fourgramsNews, fourgramsTwitter)
gc()
allFourgrams <- data.table(allFourgrams)
allFourgrams <- allFourgrams[, lapply(.SD, sum), by = ngram]
save(allFourgrams, file = "allFourgrams_clean.RData")
rm(fourgramsBlogs, fourgramsTwitter, fourgramsNews)

load("skipFiveGramsBlogs_clean.RData")
load("skipFiveGramsNews_clean.RData")
load("skipFiveGramsTwitter_clean.RData")
allSkipFiveGrams <- rbind.fill(skipFiveGramsBlogs, skipFiveGramsNews,
                               skipFiveGramsTwitter)
rm(skipFiveGramsBlogs, skipFiveGramsNews, skipFiveGramsTwitter)
gc()
allSkipFiveGrams <- data.table(allSkipFiveGrams)
allSkipFiveGrams <- allSkipFiveGrams[, lapply(.SD, sum), by = ngram]
save(allSkipFiveGrams, file = "allSkipFiveGrams_clean.RData")

load("skipSixGramsBlogs_clean.RData")
load("skipSixGramsNews_clean.RData")
load("skipSixGramsTwitter_clean.RData")
allSkipSixGrams <- rbind.fill(skipSixGramsBlogs, skipSixGramsNews,
                               skipSixGramsTwitter)
rm(skipSixGramsBlogs, skipSixGramsNews, skipSixGramsTwitter)
gc()
allSkipSixGrams <- data.table(allSkipSixGrams)
allSkipSixGrams <- allSkipSixGrams[, lapply(.SD, sum), by = ngram]
save(allSkipSixGrams, file = "allSkipSixGrams_clean.RData")



#--------------------------------------------------------------------
# German ------------------------------------------------------------
# de_Twitter <- readLines("Data\\de_DE\\de_DE.twitter.txt", encoding = "UTF-8")
# de_Twitter <- readLines("Data/de_DE/de_DE.twitter.txt", encoding = "UTF-8")
# de_News <- readLines("Data\\de_DE\\de_DE.news.txt", encoding = "UTF-8")
# de_News <- readLines("Data/de_DE/de_DE.news.txt", encoding = "UTF-8")
# de_Blogs <- readLines("Data\\de_DE\\de_DE.blogs.txt", encoding = "UTF-8")
# de_Blogs <- readLines("Data/de_DE/de_DE.blogs.txt", encoding = "UTF-8")


# Filter profanity ----------------------------------------------------------
badwords <- readLines("Data/badwords_de.txt")

de_Twitter <- readLines("Data/de_DE/de_DE.twitter.txt", encoding = "UTF-8")
badwordIndexTwitter <- sapply(de_Twitter, function(text){
      any(sapply(X = badwords, function(x) grepl(x, text)))
})
badwordIndexTwitter <- as.logical(badwordIndexTwitter)
save(badwordIndexTwitter, file = "badwordIndexTwitter_de.RData")
rm(de_Twitter)

de_News <- readLines("Data/de_DE/de_DE.news.txt", encoding = "UTF-8")
badwordIndexNews <- sapply(de_News, function(text){
      any(sapply(X = badwords, function(x) grepl(x, text)))
})
badwordIndexNews <- as.logical(badwordIndexNews)
save(badwordIndexNews, file = "badwordIndexNews_de.RData")
rm(de_News)

de_Blogs <- readLines("Data/de_DE/de_DE.blogs.txt", encoding = "UTF-8")
badwordIndexBlogs <- sapply(de_Blogs, function(text){
      any(sapply(X = badwords, function(x) grepl(x, text)))
})
badwordIndexBlogs <- as.logical(badwordIndexBlogs)
save(badwordIndexBlogs, file = "badwordIndexBlogs_de.RData")
rm(de_Blogs)

# Make n grams and skip n grams --------------------------------------------
# Twitter
load("badwordIndexTwitter_de.RData")
de_Twitter <- readLines("Data/de_DE/de_DE.twitter.txt", encoding = "UTF-8")
de_Twitter_clean <- de_Twitter[!badwordIndexTwitter]
rm(de_Twitter)
set.seed(1234)
twitterTrainIndices <- sample(seq_along(de_Twitter_clean),
                              size = round(0.6 * length(de_Twitter_clean)),
                              replace = F)
tokensTwitter <- makeNgrams(de_Twitter_clean[twitterTrainIndices],
                            encoding = "UTF-8",
                            convertToLower = F,
                            ngram = 1,
                            markSentences = F)
save(tokensTwitter, file = "tokensTwitter_clean_de.RData")
rm(tokensTwitter)
gc()
bigramsTwitter <- makeNgrams(de_Twitter_clean[twitterTrainIndices],
                             encoding = "UTF-8",
                             convertToLower = F,
                             ngram = 2,
                             markSentences = F)
save(bigramsTwitter, file = "bigramsTwitter_clean_de.RData")
gc()
trigramsTwitter <- makeNgrams(de_Twitter_clean[twitterTrainIndices],
                              encoding = "UTF-8",
                              convertToLower = F,
                              ngram = 3,
                              markSentences = F)
save(trigramsTwitter, file = "trigramsTwitter_clean_de.RData")
rm(trigramsTwitter)
gc()
fourgramsTwitter <- makeNgrams(de_Twitter_clean[twitterTrainIndices],
                               encoding = "UTF-8",
                               convertToLower = F,
                              ngram = 4,
                              markSentences = F)
save(fourgramsTwitter, file = "fourgramsTwitter_clean_de.RData")
rm(fourgramsTwitter, de_Twitter_clean)
gc()

# News
load("badwordIndexNews_de.RData")
de_News <- readLines("Data/de_DE/de_DE.news.txt", encoding = "UTF-8")
de_News_clean <- de_News[!badwordIndexNews]
rm(de_News)
set.seed(1234)
newsTrainIndices <- sample(seq_along(de_News_clean),
                              size = round(0.6 * length(de_News_clean)),
                              replace = F)
tokensNews <- makeNgrams(de_News_clean[newsTrainIndices],
                         encoding = "UTF-8",
                         convertToLower = F,
                            ngram = 1,
                            markSentences = F)
save(tokensNews, file = "tokensNews_clean_de.RData")
rm(tokensNews)
gc()
bigramsNews <- makeNgrams(de_News_clean[newsTrainIndices],
                          encoding = "UTF-8",
                          convertToLower = F,
                             ngram = 2,
                             markSentences = F)
save(bigramsNews, file = "bigramsNews_clean_de.RData")
gc()
trigramsNews <- makeNgrams(de_News_clean[newsTrainIndices],
                           encoding = "UTF-8",
                           convertToLower = F,
                              ngram = 3,
                              markSentences = F)
save(trigramsNews, file = "trigramsNews_clean_de.RData")
rm(trigramsNews)
gc()
fourgramsNews <- makeNgrams(de_News_clean[newsTrainIndices],
                            encoding = "UTF-8",
                            convertToLower = F,
                               ngram = 4,
                               markSentences = F)
save(fourgramsNews, file = "fourgramsNews_clean_de.RData")
rm(fourgramsNews, de_News_clean)
gc()


# Blogs
load("badwordIndexBlogs_de.RData")
de_Blogs <- readLines("Data/de_DE/de_DE.blogs.txt", encoding = "UTF-8")
de_Blogs_clean <- de_Blogs[!badwordIndexBlogs]
rm(de_Blogs)
set.seed(1234)
blogsTrainIndices <- sample(seq_along(de_Blogs_clean),
                              size = round(0.6 * length(de_Blogs_clean)),
                              replace = F)
tokensBlogs <- makeNgrams(de_Blogs_clean[blogsTrainIndices],
                          encoding = "UTF-8",
                          convertToLower = F,
                            ngram = 1,
                            markSentences = F)
save(tokensBlogs, file = "tokensBlogs_clean_de.RData")
rm(tokensBlogs)
gc()
bigramsBlogs <- makeNgrams(de_Blogs_clean[blogsTrainIndices],
                           encoding = "UTF-8",
                           convertToLower = F,
                             ngram = 2,
                             markSentences = F)
save(bigramsBlogs, file = "bigramsBlogs_clean_de.RData")
gc()
trigramsBlogs <- makeNgrams(de_Blogs_clean[blogsTrainIndices],
                            encoding = "UTF-8",
                            convertToLower = F,
                              ngram = 3,
                              markSentences = F)
save(trigramsBlogs, file = "trigramsBlogs_clean_de.RData")
rm(trigramsBlogs)
gc()
fourgramsBlogs <- makeNgrams(de_Blogs_clean[blogsTrainIndices],
                             encoding = "UTF-8",
                             convertToLower = F,
                               ngram = 4,
                               markSentences = F)
save(fourgramsBlogs, file = "fourgramsBlogs_clean_de.RData")
rm(fourgramsBlogs, de_Blogs_clean)
gc()


# Skip-n-grams -------------------------------------------------------
# Twitter
load("badwordIndexTwitter_de.RData")
de_Twitter <- readLines("Data/de_DE/de_DE.twitter.txt", encoding = "UTF-8")
de_Twitter_clean <- de_Twitter[!badwordIndexTwitter]
rm(de_Twitter)
gc()
skipFiveGramsTwitter <- makeNgrams(de_Twitter_clean[twitterTrainIndices],
                                   skip = T,
                                   encoding = "UTF-8",
                                   convertToLower = F,
                            ngram = 5,
                            markSentences = F)
save(skipFiveGramsTwitter, file = "skipFiveGramsTwitter_clean_de.RData")
rm(skipFiveGramsTwitter)
gc()
skipSixGramsTwitter <- makeNgrams(de_Twitter_clean[twitterTrainIndices],
                                  skip = T,
                                  encoding = "UTF-8",
                                  convertToLower = F,
                             ngram = 6,
                             markSentences = F)
save(skipSixGramsTwitter, file = "skipSixGramsTwitter_clean_de.RData")
rm(skipSixGramsTwitter)
gc()

# News
load("badwordIndexNews_de.RData")
de_News <- readLines("Data/de_DE/de_DE.news.txt", encoding = "UTF-8")
de_News_clean <- de_News[!badwordIndexNews]
rm(de_News)
gc()
skipFiveGramsNews <- makeNgrams(de_News_clean[newsTrainIndices],
                                   skip = T,
                                encoding = "UTF-8",
                                convertToLower = F,
                            ngram = 5,
                            markSentences = F)
save(skipFiveGramsNews, file = "skipFiveGramsNews_clean_de.RData")
rm(skipFiveGramsNews)
gc()
skipSixGramsNews <- makeNgrams(de_News_clean[newsTrainIndices],
                                  skip = T,
                               encoding = "UTF-8",
                               convertToLower = F,
                             ngram = 6,
                             markSentences = F)
save(skipSixGramsNews, file = "skipSixGramsNews_clean_de.RData")
rm(skipSixGramsNews)
gc()

# Blogs
load("badwordIndexBlogs_de.RData")
de_Blogs <- readLines("Data/de_DE/de_DE.blogs.txt", encoding = "UTF-8")
de_Blogs_clean <- de_Blogs[!badwordIndexBlogs]
rm(de_Blogs)
gc()
skipFiveGramsBlogs <- makeNgrams(de_Blogs_clean[blogsTrainIndices],
                                   skip = T,
                                 encoding = "UTF-8",
                                 convertToLower = F,
                            ngram = 5,
                            markSentences = F)
save(skipFiveGramsBlogs, file = "skipFiveGramsBlogs_clean_de.RData")
rm(skipFiveGramsBlogs)
gc()
skipSixGramsBlogs <- makeNgrams(de_Blogs_clean[blogsTrainIndices],
                                  skip = T,
                                encoding = "UTF-8",
                                convertToLower = F,
                             ngram = 6,
                             markSentences = F)
save(skipSixGramsBlogs, file = "skipSixGramsBlogs_clean_de.RData")
rm(skipSixGramsBlogs)
gc()


# Combine n-grams of the different sources ------------------------------------
load("tokensBlogs_clean_de.RData")
load("tokensNews_clean_de.RData")
load("tokensTwitter_clean_de.RData")
allTokens <- rbind.fill(tokensBlogs, tokensNews, tokensTwitter)
allTokens <- data.table(allTokens)
allTokens <- allTokens[, lapply(.SD, sum), by = ngram]
save(allTokens, file = "allTokens_clean_de.RData")
rm(tokensBlogs, tokensTwitter, tokensNews, allTokens)
gc()

load("bigramsBlogs_clean_de.RData")
load("bigramsNews_clean_de.RData")
load("bigramsTwitter_clean_de.RData")
allBigrams <- rbind.fill(bigramsBlogs, bigramsNews, bigramsTwitter)
allBigrams <- data.table(allBigrams)
allBigrams <- allBigrams[, lapply(.SD, sum), by = ngram]
save(allBigrams, file = "allBigrams_clean_de.RData")
rm(bigramsBlogs, bigramsTwitter, bigramsNews, allBigrams)
gc()

load("trigramsBlogs_clean_de.RData")
load("trigramsNews_clean_de.RData")
load("trigramsTwitter_clean_de.RData")
allTrigrams <- rbind.fill(trigramsBlogs, trigramsNews, trigramsTwitter)
allTrigrams <- data.table(allTrigrams)
allTrigrams <- allTrigrams[, lapply(.SD, sum), by = ngram]
save(allTrigrams, file = "allTrigrams_clean_de.RData")
rm(trigramsBlogs, trigramsTwitter, trigramsNews)
gc()

load("fourgramsBlogs_clean_de.RData")
load("fourgramsNews_clean_de.RData")
load("fourgramsTwitter_clean_de.RData")
allFourgrams <- rbind.fill(fourgramsBlogs, fourgramsNews, fourgramsTwitter)
rm(fourgramsBlogs, fourgramsNews, fourgramsTwitter)
gc()
allFourgrams <- data.table(allFourgrams)
allFourgrams <- allFourgrams[, lapply(.SD, sum), by = ngram]
save(allFourgrams, file = "allFourgrams_clean_de.RData")

load("skipFiveGramsBlogs_clean_de.RData")
load("skipFiveGramsNews_clean_de.RData")
load("skipFiveGramsTwitter_clean_de.RData")
allSkipFiveGrams <- rbind.fill(skipFiveGramsBlogs, skipFiveGramsNews,
                               skipFiveGramsTwitter)
rm(skipFiveGramsBlogs, skipFiveGramsNews, skipFiveGramsTwitter)
gc()
allSkipFiveGrams <- data.table(allSkipFiveGrams)
allSkipFiveGrams <- allSkipFiveGrams[, lapply(.SD, sum), by = ngram]
save(allSkipFiveGrams, file = "allSkipFiveGrams_clean_de.RData")

load("skipSixGramsBlogs_clean_de.RData")
load("skipSixGramsNews_clean_de.RData")
load("skipSixGramsTwitter_clean_de.RData")
allSkipSixGrams <- rbind.fill(skipSixGramsBlogs, skipSixGramsNews,
                              skipSixGramsTwitter)
rm(skipSixGramsBlogs, skipSixGramsNews, skipSixGramsTwitter)
gc()
allSkipSixGrams <- data.table(allSkipSixGrams)
allSkipSixGrams <- allSkipSixGrams[, lapply(.SD, sum), by = ngram]
save(allSkipSixGrams, file = "allSkipSixGrams_clean_de.RData")
