library(beepr)
library(data.table)
library(tau)
library(plyr)

en_Twitter <- readLines("Data\\en_US\\en_US.twitter.txt", encoding = "UTF-8")
en_Twitter <- readLines("Data/en_US/en_US.twitter.txt", encoding = "UTF-8")
en_News <- readLines("Data\\en_US\\en_US.news.txt", encoding = "UTF-8")
en_News <- readLines("Data/en_US/en_US.news.txt", encoding = "UTF-8")
en_Blogs <- readLines("Data\\en_US\\en_US.blogs.txt", encoding = "UTF-8")
en_Blogs <- readLines("Data/en_US/en_US.blogs.txt", encoding = "UTF-8")

# create n-grams in "Capstone create n-grams.R"

#-----------------------------------------------------------------------------

# Simple function to predict the next word based on unsmoothed n-gram probabilities
load("allTrigrams.RData")
load("allBigrams.RData")
load("allTokens.RData")
allTrigrams2 <- allTrigrams[count > 2]
allBigrams2 <- allBigrams[count > 2]
allTokens2 <- allTokens[count > 2]

# Split ngram into first word(s) and the next word
trigrams <- do.call(rbind, strsplit(allTrigrams2$ngram, split = " "))
trigrams <- cbind(apply(trigrams[, 1:2], 1, function(x) paste(x, collapse = " ")),
                  trigrams[, 3])
allTrigrams2$ngram <- trigrams[, 1]
allTrigrams2$nextword <- trigrams[, 2]
# Exclude trigrams in which ' is a word
delete <- grep(pattern = "' ", allTrigrams2$ngram)
allTrigrams2 <- allTrigrams2[-delete, ]

bigrams <- do.call(rbind, strsplit(allBigrams2$ngram, split = " "))
allBigrams2$ngram <- bigrams[, 1]
allBigrams2$nextword <- bigrams[, 2]
# Exclude bigrams in which ' is a word
allBigrams2 <- allBigrams2[-(which(allBigrams2$ngram == "'")), ]

setkey(allTokens2, "count", "ngram")
setkey(allBigrams2, "count", "ngram")
setkey(allTrigrams2, "count", "ngram")

predictSimple <- function(input, tokens, bigrams, trigrams){
      input <- tolower(input)
      splitInput <- unlist(strsplit(input, split = " "))
      if (length(splitInput) > 2){
            input2 <- paste(tail(splitInput, 2), collapse = " ")
      } else {
            input2 <- paste(splitInput, collapse = " ")
      }
      trigramPred <- tail(trigrams[ngram == input2]$nextword, 10)
      if (length(trigramPred > 0)){
            print("trigramPred")
            return(trigramPred)
      } else {
            input2 <- tail(splitInput, 1)
            bigramPred <- tail(bigrams[ngram == input2]$nextword, 10)
            if (length(bigramPred) > 0){
                  print("bigramPred")
                  return(bigramPred)
            } else {
                  unigramPred <- tail(tokens[ngram == input2]$nextword, 10)
                  if (length(unigramPred) > 0){
                        print("unigramPred")
                        return(unigramPred)
                  } else {
                        tokenPred <- tail(tokens$ngram, 1)
                        print("tokenPred")
                        return(tokenPred)
                  }
            }
      }
}

predictSimple("Adam Sandler is the only", allTokens2, allBigrams2, allTrigrams2)


#--------------------------------------------------------------------------
# Kneser-Ney smoothing. Convert "tables" of counts to Kneser-Ney-smoothed
# probabilities

### Bigrams
load("allBigrams_clean.RData")
allBigramsKN <- allBigrams[count > 1]
bigrams <- do.call(rbind, strsplit(allBigramsKN$ngram, split = " "))
allBigramsKN$ngram <- bigrams[, 1]
allBigramsKN$nextword <- bigrams[, 2]
# Exclude bigrams with symbols as words
delete <- grep("['+\\/+<+>+]", allBigramsKN$ngram)
allBigramsKN <- allBigramsKN[-delete, ]
# rm(bigrams)
setkey(allBigramsKN, "count", "ngram")
# Discounting
allBigramsKN[, D := 0]
Y <- (nrow(allBigramsKN[count == 2]) /
            (nrow(allBigramsKN[count == 2]) + 2 * nrow(allBigramsKN[count == 3])))
# = 0.54
allBigramsKN[count == 1]$D <- 1 - 2 * Y * (nrow(allBigramsKN[count == 2]) /
                                                 nrow(allBigramsKN[count == 1]))
# 0.7
allBigramsKN[count == 2]$D <- 2 - 3 * Y * (nrow(allBigramsKN[count == 3]) /
                                                 nrow(allBigramsKN[count == 2]))
# 1.01
allBigramsKN[count > 2]$D <- 3 - 4 * Y * (nrow(allBigramsKN[count == 4]) /
                                                nrow(allBigramsKN[count == 3]))
# 1.4
allBigramsKN[, count := count - D]
# divide by c(w_{i-1}): Count of allBigramsKN$ngram (from allTokens)
load("allTokens_clean.RData")
allBigramsKN <- merge(allBigramsKN, allTokens, by = "ngram")
setnames(allBigramsKN, c("ngram", "count", "nextword", "D", "ngramcount"))
allBigramsKN[, count := count / ngramcount]
# calculate lambda
allBigramsKN[, lambda := D / ngramcount]
NNextwords <- allBigramsKN[, .(ngram.NNextwords = length(nextword)), by = ngram]
allBigramsKN <- merge(allBigramsKN, NNextwords, by = "ngram")
rm(NNextwords)
allBigramsKN[, lambda := lambda * ngram.NNextwords]
# calculate P(continuation)
NNewCont <- allBigramsKN[, .(nextword.NNewCont = length(ngram)), by = nextword]
allBigramsKN <- merge(allBigramsKN, NNewCont, by = "nextword")
allBigramsKN[, Pcont := nextword.NNewCont / nrow(allBigramsKN)]
# add lambda * Pcont to the term for P_{KN}
allBigramsKN[, count := count + lambda * Pcont]
save(allBigramsKN, file = "allBigramsKN.RData")

### Trigrams
load("allTrigrams_clean.RData")
allTrigrams <- allTrigrams[count > 1] # saves 1.5GB
save(allTrigrams, file = "allTrigrams_clean_pruned.RData")
load("allTrigrams_clean_pruned.RData")
allTrigramsKN <- allTrigrams
rm(allTrigrams); gc()
trigrams <- do.call(rbind, strsplit(allTrigramsKN$ngram, split = " "))
allTrigramsKN$ngram <-  apply(trigrams[, 1:2], 1,
                              FUN = function(x) paste(x, collapse = " "))
allTrigramsKN$nextword <- trigrams[, 3]
# Exclude trigrams with symbols as words
delete <- grep("['+\\/+<+>+]", allTrigramsKN$ngram)
allTrigramsKN <- allTrigramsKN[-delete, ]
rm(trigrams)
setkey(allTrigramsKN, "count", "ngram")
# Discounting
allTrigramsKN[, D := 0]
# using trigrams with count = 2 or 3
Y <- (nrow(allTrigramsKN[count == 2]) /
            (nrow(allTrigramsKN[count == 2]) + 2 * nrow(allTrigramsKN[count == 3])))
# = 0.6
allTrigramsKN[count == 2]$D <- 2 - 3 * Y * (nrow(allTrigramsKN[count == 3]) /
                                                  nrow(allTrigramsKN[count == 2]))
allTrigramsKN[count > 2]$D <- 3 - 4 * Y * (nrow(allTrigramsKN[count == 4]) /
                                                 nrow(allTrigramsKN[count == 3]))
allTrigramsKN[, count := count - D]
# divide by c(w_{i-1}): Count of allTrigramsKN$ngram (from allBigrams)
load("allBigrams_clean.RData")
allTrigramsKN <- merge(allTrigramsKN, allBigrams, by = "ngram")
setnames(allTrigramsKN, c("ngram", "count", "nextword", "D", "ngramcount"))
allTrigramsKN[, count := count / ngramcount]
rm(allBigrams); gc()
# calculate lambda
allTrigramsKN[, lambda := D / ngramcount]
NNextwords <- allTrigramsKN[, .(ngram.NNextwords = length(nextword)), by = ngram]
allTrigramsKN <- merge(allTrigramsKN, NNextwords, by = "ngram")
rm(NNextwords)
allTrigramsKN[, lambda := lambda * ngram.NNextwords]
# P_{KN}(wi | w^{i-1}_{i-n+2}), i.e. P_{KN} of the last word of ngram and nextword
# from allBigramsKN
# 2. part of the n-gram for the table of bigrams
load("allBigramsKN.RData")
lowerNgram <- as.character(sapply(allTrigramsKN$ngram,
                                  function(x) unlist(strsplit(x, split = " "))[2]))
allTrigramsKN <- cbind(allTrigramsKN, lowerNgram)
allBigramsKN2 <- allBigramsKN[, .(ngram, nextword, count)]
rm(allBigramsKN); gc()
setnames(allBigramsKN2, c("lowerNgram", "nextword", "count"))
allTrigramsKN <- merge(allTrigramsKN, allBigramsKN2, by = c("lowerNgram", "nextword"))
setnames(allTrigramsKN, c("lowerNgram", "nextword", "ngram", "count",
                          "D", "ngramcount", "lambda", "ngram.NNextwords",
                          "PknLower"))
# add lambda * PknLower to the term for P_{KN}
allTrigramsKN[, count := count + lambda * PknLower]
save(allTrigramsKN, file = "allTrigramsKN.RData")


### Fourgrams
load("allFourgrams_clean.RData")
allFourgrams <- allFourgrams[count > 1] # saves 1,8GB
save(allFourgrams, file = "allFourgrams_clean_pruned.RData")
load("allFourgrams_clean_pruned.RData")
allFourgramsKN <- allFourgrams
rm(allFourgrams); gc()
fourgrams <- do.call(rbind, strsplit(allFourgramsKN$ngram, split = " "))
allFourgramsKN$ngram <-  apply(fourgrams[, 1:3], 1,
                               FUN = function(x) paste(x, collapse = " "))
allFourgramsKN$nextword <- fourgrams[, 4]
# Exclude fourgrams with symbols as words
delete <- grep("['+\\/+<+>+]", allFourgramsKN$ngram)
allFourgramsKN <- allFourgramsKN[-delete, ]
rm(fourgrams)
setkey(allFourgramsKN, "count", "ngram")
# Discounting
allFourgramsKN[, D := 0]
Y <- (nrow(allFourgramsKN[count == 2]) /
            (nrow(allFourgramsKN[count == 2]) + 2 * nrow(allFourgramsKN[count == 3])))
# = 0.6
allFourgramsKN[count == 2]$D <- 2 - 3 * Y * (nrow(allFourgramsKN[count == 3]) /
                                                   nrow(allFourgramsKN[count == 2]))
allFourgramsKN[count > 2]$D <- 3 - 4 * Y * (nrow(allFourgramsKN[count == 4]) /
                                                  nrow(allFourgramsKN[count == 3]))
allFourgramsKN[, count := count - D]
# divide by c(w_{i-1}): Count of allFourgramsKN$ngram (from allTrigrams)
load("allTrigrams_clean_pruned.RData")
allFourgramsKN <- merge(allFourgramsKN, allTrigrams, by = "ngram")
setnames(allFourgramsKN, c("ngram", "count", "nextword", "D", "ngramcount"))
allFourgramsKN[, count := count / ngramcount]
rm(allTrigrams); gc()
# lambda
allFourgramsKN[, lambda := D / ngramcount]
NNextwords <- allFourgramsKN[, .(ngram.NNextwords = length(nextword)), by = ngram]
allFourgramsKN <- merge(allFourgramsKN, NNextwords, by = "ngram")
rm(NNextwords)
allFourgramsKN[, lambda := lambda * ngram.NNextwords]
# P_{KN}(wi | w^{i-1}_{i-n+2}), i.e. P_{KN} of the last word of ngram and nextword
# from allTrigramsKN
# 2. part of the n-grams as n-gram for the table of Trigrams
load("allTrigramsKN.RData")
lowerNgram <- as.character(sapply(allFourgramsKN$ngram,
                                  function(x){
                                        paste(unlist(strsplit(x, split = " "))[2:3],
                                              collapse = " ")}
))
allFourgramsKN <- cbind(allFourgramsKN, lowerNgram)
allTrigramsKN2 <- allTrigramsKN[, .(ngram, nextword, count)]
rm(allTrigramsKN); gc()
setnames(allTrigramsKN2, c("lowerNgram", "nextword", "count"))
allFourgramsKN <- merge(allFourgramsKN, allTrigramsKN2,
                        by = c("lowerNgram", "nextword"))
setnames(allFourgramsKN, c("lowerNgram", "nextword", "ngram", "count",
                           "D", "ngramcount", "lambda", "ngram.NNextwords",
                           "PknLower"))
# add lambda * PknLower to the term for P_{KN}
allFourgramsKN[, count := count + lambda * PknLower]
save(allFourgramsKN, file = "allFourgramsKN.RData")


# Kneser-Ney-Smoothing German -----------------------------------------------
### Bigrams
load("allBigrams_clean_de.RData")
allBigramsKN <- allBigrams[count > 1]
bigrams <- do.call(rbind, strsplit(allBigramsKN$ngram, split = " "))
allBigramsKN$ngram <- bigrams[, 1]
allBigramsKN$nextword <- bigrams[, 2]
# Exclude bigrams with symbols as words
delete <- grep("['+\\/+<+>+]", allBigramsKN$ngram)
allBigramsKN <- allBigramsKN[-delete, ]
# rm(bigrams)
setkey(allBigramsKN, "count", "ngram")
# Discounting
allBigramsKN[, D := 0]
Y <- (nrow(allBigramsKN[count == 2]) /
            (nrow(allBigramsKN[count == 2]) + 2 * nrow(allBigramsKN[count == 3])))
# = 0.54
allBigramsKN[count == 1]$D <- 1 - 2 * Y * (nrow(allBigramsKN[count == 2]) /
                                                 nrow(allBigramsKN[count == 1]))
# 0.7
allBigramsKN[count == 2]$D <- 2 - 3 * Y * (nrow(allBigramsKN[count == 3]) /
                                                 nrow(allBigramsKN[count == 2]))
# 1.01
allBigramsKN[count > 2]$D <- 3 - 4 * Y * (nrow(allBigramsKN[count == 4]) /
                                                nrow(allBigramsKN[count == 3]))
# 1.4
allBigramsKN[, count := count - D]
# divide by c(w_{i-1}): Count of allBigramsKN$ngram (from allTokens)
load("allTokens_clean_de.RData")
allBigramsKN <- merge(allBigramsKN, allTokens, by = "ngram")
setnames(allBigramsKN, c("ngram", "count", "nextword", "D", "ngramcount"))
allBigramsKN[, count := count / ngramcount]
# lambda
allBigramsKN[, lambda := D / ngramcount]
NNextwords <- allBigramsKN[, .(ngram.NNextwords = length(nextword)), by = ngram]
allBigramsKN <- merge(allBigramsKN, NNextwords, by = "ngram")
rm(NNextwords)
allBigramsKN[, lambda := lambda * ngram.NNextwords]
# P(continuation)
NNewCont <- allBigramsKN[, .(nextword.NNewCont = length(ngram)), by = nextword]
allBigramsKN <- merge(allBigramsKN, NNewCont, by = "nextword")
allBigramsKN[, Pcont := nextword.NNewCont / nrow(allBigramsKN)]
# add lambda * Pcont to the term for  P_{KN}
allBigramsKN[, count := count + lambda * Pcont]
save(allBigramsKN, file = "allBigramsKN_de.RData")

### Trigrams
load("allTrigrams_clean_de.RData")
allTrigramsKN <- allTrigrams
rm(allTrigrams); gc()
trigrams <- do.call(rbind, strsplit(allTrigramsKN$ngram, split = " "))
allTrigramsKN$ngram <-  apply(trigrams[, 1:2], 1,
                              FUN = function(x) paste(x, collapse = " "))
allTrigramsKN$nextword <- trigrams[, 3]
# Exclude trigrams with symbols as words
delete <- grep("['+\\/+<+>+]", allTrigramsKN$ngram)
allTrigramsKN <- allTrigramsKN[-delete, ]
rm(trigrams)
setkey(allTrigramsKN, "count", "ngram")
# Discounting
allTrigramsKN[, D := 0]
# count 2 or 3
Y <- (nrow(allTrigramsKN[count == 2]) /
            (nrow(allTrigramsKN[count == 2]) + 2 * nrow(allTrigramsKN[count == 3])))
# = 0.6
allTrigramsKN[count == 2]$D <- 2 - 3 * Y * (nrow(allTrigramsKN[count == 3]) /
                                                  nrow(allTrigramsKN[count == 2]))
allTrigramsKN[count > 2]$D <- 3 - 4 * Y * (nrow(allTrigramsKN[count == 4]) /
                                                 nrow(allTrigramsKN[count == 3]))
allTrigramsKN[, count := count - D]
# divide by c(w_{i-1}): Count of allTrigramsKN$ngram (from allBigrams)
load("allBigrams_clean_de.RData")
allTrigramsKN <- merge(allTrigramsKN, allBigrams, by = "ngram")
setnames(allTrigramsKN, c("ngram", "count", "nextword", "D", "ngramcount"))
allTrigramsKN[, count := count / ngramcount]
rm(allBigrams); gc()
# calculate lambda
allTrigramsKN[, lambda := D / ngramcount]
NNextwords <- allTrigramsKN[, .(ngram.NNextwords = length(nextword)), by = ngram]
allTrigramsKN <- merge(allTrigramsKN, NNextwords, by = "ngram")
rm(NNextwords)
allTrigramsKN[, lambda := lambda * ngram.NNextwords]
# P_{KN}(wi | w^{i-1}_{i-n+2}), i.e. P_{KN} of the last word of ngram and nextword
# from allBigramsKN
# 2. Teil of the n-grams as n-gram for the table of bigrams
load("allBigramsKN_de.RData")
lowerNgram <- as.character(sapply(allTrigramsKN$ngram,
                                  function(x) unlist(strsplit(x, split = " "))[2]))
allTrigramsKN <- cbind(allTrigramsKN, lowerNgram)
allBigramsKN2 <- allBigramsKN[, .(ngram, nextword, count)]
rm(allBigramsKN); gc()
setnames(allBigramsKN2, c("lowerNgram", "nextword", "count"))
allTrigramsKN <- merge(allTrigramsKN, allBigramsKN2, by = c("lowerNgram", "nextword"))
setnames(allTrigramsKN, c("lowerNgram", "nextword", "ngram", "count",
                          "D", "ngramcount", "lambda", "ngram.NNextwords",
                          "PknLower"))
# add lambda * PknLower to the term for P_{KN}
allTrigramsKN[, count := count + lambda * PknLower]
save(allTrigramsKN, file = "allTrigramsKN_de.RData")


### Fourgrams
load("allFourgrams_clean_de.RData")
allFourgramsKN <- allFourgrams
rm(allFourgrams); gc()
fourgrams <- do.call(rbind, strsplit(allFourgramsKN$ngram, split = " "))
allFourgramsKN$ngram <-  apply(fourgrams[, 1:3], 1,
                               FUN = function(x) paste(x, collapse = " "))
allFourgramsKN$nextword <- fourgrams[, 4]
# Exclude fourgrams with symbols as words
delete <- grep("['+\\/+<+>+]", allFourgramsKN$ngram)
allFourgramsKN <- allFourgramsKN[-delete, ]
rm(fourgrams)
setkey(allFourgramsKN, "count", "ngram")
# Discounting
allFourgramsKN[, D := 0]
# count 2 und 3
Y <- (nrow(allFourgramsKN[count == 2]) /
            (nrow(allFourgramsKN[count == 2]) + 2 * nrow(allFourgramsKN[count == 3])))
# = 0.6
allFourgramsKN[count == 2]$D <- 2 - 3 * Y * (nrow(allFourgramsKN[count == 3]) /
                                                   nrow(allFourgramsKN[count == 2]))
allFourgramsKN[count > 2]$D <- 3 - 4 * Y * (nrow(allFourgramsKN[count == 4]) /
                                                  nrow(allFourgramsKN[count == 3]))
allFourgramsKN[, count := count - D]
# divide by c(w_{i-1}): Count of allFourgramsKN$ngram (from allTrigrams)
load("allTrigrams_clean_de.RData")
allFourgramsKN <- merge(allFourgramsKN, allTrigrams, by = "ngram")
setnames(allFourgramsKN, c("ngram", "count", "nextword", "D", "ngramcount"))
allFourgramsKN[, count := count / ngramcount]
rm(allTrigrams); gc()
# lambda
allFourgramsKN[, lambda := D / ngramcount]
NNextwords <- allFourgramsKN[, .(ngram.NNextwords = length(nextword)), by = ngram]
allFourgramsKN <- merge(allFourgramsKN, NNextwords, by = "ngram")
rm(NNextwords)
allFourgramsKN[, lambda := lambda * ngram.NNextwords]
# P_{KN}(wi | w^{i-1}_{i-n+2}), i.e. P_{KN} of the last word of ngram and nextword
# from allTrigramsKN
# 2. part of the n-grams as n-gram for the table of trigrams
load("allTrigramsKN_de.RData")
lowerNgram <- as.character(sapply(allFourgramsKN$ngram,
                                  function(x){
                                        paste(unlist(strsplit(x, split = " "))[2:3],
                                              collapse = " ")}
))
allFourgramsKN <- cbind(allFourgramsKN, lowerNgram)
allTrigramsKN2 <- allTrigramsKN[, .(ngram, nextword, count)]
rm(allTrigramsKN); gc()
setnames(allTrigramsKN2, c("lowerNgram", "nextword", "count"))
allFourgramsKN <- merge(allFourgramsKN, allTrigramsKN2,
                        by = c("lowerNgram", "nextword"))
setnames(allFourgramsKN, c("lowerNgram", "nextword", "ngram", "count",
                           "D", "ngramcount", "lambda", "ngram.NNextwords",
                           "PknLower"))
# add lambda * PknLower to the term for P_{KN}
allFourgramsKN[, count := count + lambda * PknLower]
save(allFourgramsKN, file = "allFourgramsKN_de.RData")


#----------------------------------------------------------------------------
# Function to predict the next word based on Kneser-Ney n-gram probabilities
# or simple counts using a backoff model. The Kneser-Ney model backs off to
# token probabilities if the n-gram is not known

# Save all counts in probabilities in one list of data.tables
# English
load("allTrigrams_clean_pruned.RData")
load("allBigrams_clean.RData")
load("allTokens_clean.RData")
allTokens_en <- allTokens[count > 3]
allBigrams_en <- allBigrams[count > 3]
allTrigrams_en <- allTrigrams[count > 3]
rm(allTokens, allBigrams, allTrigrams)
load("allFourgrams_clean_pruned.RData")
allFourgrams_en <- allFourgrams[count > 2]
rm(allFourgrams)

# Split ngram into first word(s) and the next word
# for the data.tables of simple counts
fourgrams <- do.call(rbind, strsplit(allFourgrams_en$ngram, split = " "))
fourgrams <- cbind(apply(fourgrams[, 1:3], 1, function(x) paste(x, collapse = " ")),
                   fourgrams[, 4])
allFourgrams_en$ngram <- fourgrams[, 1]
allFourgrams_en$nextword <- fourgrams[, 2]
# Exclude fourgrams in which ' is a word
delete <- grep(pattern = "^'+", allFourgrams_en$ngram)
allFourgrams_en <- allFourgrams_en[-delete, ]
delete <- grep(pattern = "^'+", allFourgrams_en$nextword)
allFourgrams_en <- allFourgrams_en[-delete, ]

trigrams <- do.call(rbind, strsplit(allTrigrams_en$ngram, split = " "))
trigrams <- cbind(apply(trigrams[, 1:2], 1, function(x) paste(x, collapse = " ")),
                  trigrams[, 3])
allTrigrams_en$ngram <- trigrams[, 1]
allTrigrams_en$nextword <- trigrams[, 2]
# Exclude trigrams in which ' is a word
delete <- grep(pattern = "^'+", allTrigrams_en$ngram)
allTrigrams_en <- allTrigrams_en[-delete, ]
delete <- grep(pattern = "^'+", allTrigrams_en$nextword)
allTrigrams_en <- allTrigrams_en[-delete, ]

bigrams <- do.call(rbind, strsplit(allBigrams_en$ngram, split = " "))
allBigrams_en$ngram <- bigrams[, 1]
allBigrams_en$nextword <- bigrams[, 2]
# Exclude bigrams in which ' is a word
delete <- grep(pattern = "^'+", allBigrams_en$ngram)
allBigrams_en <- allBigrams_en[-delete, ]
delete <- grep(pattern = "^'+", allBigrams_en$nextword)
allBigrams_en <- allBigrams_en[-delete, ]

# KN smoothed english
load("allFourgramsKN.RData")
load("allTrigramsKN.RData")
allTrigramsKN_en <- allTrigramsKN[, .(ngram, nextword, count)]
allFourgramsKN_en <- allFourgramsKN[, .(ngram, nextword, count)]
rm(allTrigramsKN, allFourgramsKN)
load("allBigramsKN.RData")
allBigramsKN_en <- allBigramsKN[, .(ngram, nextword, count)]
rm(allBigramsKN)
# Exclude n-grams that start with '
delete <- grep(pattern = "^'+", allBigramsKN_en$ngram)
delete <- c(delete, grep(pattern = "^'+", allBigramsKN_en$nextword))
allBigramsKN_en <- allBigramsKN_en[-delete, ]
delete <- grep(pattern = "^'+", allTrigramsKN_en$ngram)
delete <- c(delete, grep(pattern = "^'+", allTrigramsKN_en$nextword))
allTrigramsKN_en <- allTrigramsKN_en[-delete, ]
delete <- grep(pattern = "^'+", allFourgramsKN_en$ngram)
delete <- c(delete, grep(pattern = "^'+", allFourgramsKN_en$nextword))
allFourgramsKN_en <- allFourgramsKN_en[-delete, ]

rm(bigrams, trigrams, fourgrams)
setkey(allTokens_en, "count", "ngram")
setkey(allBigrams_en, "count", "ngram")
setkey(allTrigrams_en, "count", "ngram")
setkey(allFourgrams_en, "count", "ngram")
setkey(allBigramsKN_en, "count", "ngram")
setkey(allTrigramsKN_en, "count", "ngram")
setkey(allFourgramsKN_en, "count", "ngram")

### German
load("allFourgrams_clean_de.RData")
load("allTrigrams_clean_de.RData")
load("allBigrams_clean_de.RData")
load("allTokens_clean_de.RData")
allTokens_de <- allTokens[count > 3]
allBigrams_de <- allBigrams[count > 3]
allTrigrams_de <- allTrigrams[count > 3]
allFourgrams_de <- allFourgrams[count > 2]
rm(allTokens, allBigrams, allTrigrams, allFourgrams)

# Split ngram into first word(s) and the next word
# for the data.tables of simple counts
fourgrams <- do.call(rbind, strsplit(allFourgrams_de$ngram, split = " "))
fourgrams <- cbind(apply(fourgrams[, 1:3], 1, function(x) paste(x, collapse = " ")),
                   fourgrams[, 4])
allFourgrams_de$ngram <- fourgrams[, 1]
allFourgrams_de$nextword <- fourgrams[, 2]
# Exclude fourgrams in which ' is a word
delete <- grep(pattern = "^'+", allFourgrams_de$ngram)
allFourgrams_de <- allFourgrams_de[-delete, ]
delete <- grep(pattern = "^'+", allFourgrams_de$nextword)
allFourgrams_de <- allFourgrams_de[-delete, ]

trigrams <- do.call(rbind, strsplit(allTrigrams_de$ngram, split = " "))
trigrams <- cbind(apply(trigrams[, 1:2], 1, function(x) paste(x, collapse = " ")),
                  trigrams[, 3])
allTrigrams_de$ngram <- trigrams[, 1]
allTrigrams_de$nextword <- trigrams[, 2]
# Exclude trigrams in which ' is a word
delete <- grep(pattern = "^'+", allTrigrams_de$ngram)
allTrigrams_de <- allTrigrams_de[-delete, ]
delete <- grep(pattern = "^'+", allTrigrams_de$nextword)
allTrigrams_de <- allTrigrams_de[-delete, ]

bigrams <- do.call(rbind, strsplit(allBigrams_de$ngram, split = " "))
allBigrams_de$ngram <- bigrams[, 1]
allBigrams_de$nextword <- bigrams[, 2]
# Exclude bigrams in which ' is a word
delete <- grep(pattern = "^'+", allBigrams_de$ngram)
allBigrams_de <- allBigrams_de[-delete, ]
delete <- grep(pattern = "^'+", allBigrams_de$nextword)
allBigrams_de <- allBigrams_de[-delete, ]


# KN smoothed
load("allFourgramsKN_de.RData")
load("allTrigramsKN_de.RData")
allTrigramsKN_de <- allTrigramsKN[, .(ngram, nextword, count)]
allFourgramsKN_de <- allFourgramsKN[, .(ngram, nextword, count)]
rm(allTrigramsKN, allFourgramsKN)
load("allBigramsKN_de.RData")
allBigramsKN_de <- allBigramsKN[, .(ngram, nextword, count)]
rm(allBigramsKN)
# Exclude n-grams that start with '
delete <- grep(pattern = "^'+", allBigramsKN_de$ngram)
delete <- c(delete, grep(pattern = "^'+", allBigramsKN_de$nextword))
allBigramsKN_de <- allBigramsKN_de[-delete, ]
delete <- grep(pattern = "^'+", allTrigramsKN_de$ngram)
delete <- c(delete, grep(pattern = "^'+", allTrigramsKN_de$nextword))
allTrigramsKN_de <- allTrigramsKN_de[-delete, ]
delete <- grep(pattern = "^'+", allFourgramsKN_de$ngram)
delete <- c(delete, grep(pattern = "^'+", allFourgramsKN_de$nextword))
allFourgramsKN_de <- allFourgramsKN_de[-delete, ]

rm(bigrams, trigrams, fourgrams)
setkey(allTokens_de, "count", "ngram")
setkey(allBigrams_de, "count", "ngram")
setkey(allTrigrams_de, "count", "ngram")
setkey(allFourgrams_de, "count", "ngram")
setkey(allBigramsKN_de, "count", "ngram")
setkey(allTrigramsKN_de, "count", "ngram")
setkey(allFourgramsKN_de, "count", "ngram")

### Skip-n-grams German and English
load("allSkipFiveGrams_clean.RData")
bigrams <- do.call(rbind, strsplit(allSkipFiveGrams$ngram, split = " "))
allSkipFiveGrams$ngram <- bigrams[, 1]
allSkipFiveGrams$nextword <- bigrams[, 2]
delete <- grep(pattern = "^'+", allSkipFiveGrams$ngram)
delete <- c(delete, grep(pattern = "^'+", allSkipFiveGrams$nextword))
delete <- c(delete, grep(pattern = "<", allSkipFiveGrams$nextword))
allSkipFiveGrams_en <- allSkipFiveGrams[-unique(delete), ]
load("allSkipSixGrams_clean.RData")
bigrams <- do.call(rbind, strsplit(allSkipSixGrams$ngram, split = " "))
allSkipSixGrams$ngram <- bigrams[, 1]
allSkipSixGrams$nextword <- bigrams[, 2]
delete <- grep(pattern = "^'+", allSkipSixGrams$ngram)
delete <- c(delete, grep(pattern = "^'+", allSkipSixGrams$nextword))
delete <- c(delete, grep(pattern = "<", allSkipSixGrams$nextword))
allSkipSixGrams_en <- allSkipSixGrams[-unique(delete), ]
load("allSkipFiveGrams_clean_de.RData")
bigrams <- do.call(rbind, strsplit(allSkipFiveGrams$ngram, split = " "))
allSkipFiveGrams$ngram <- bigrams[, 1]
allSkipFiveGrams$nextword <- bigrams[, 2]
delete <- grep(pattern = "^'+", allSkipFiveGrams$ngram)
delete <- c(delete, grep(pattern = "^'+", allSkipFiveGrams$nextword))
delete <- c(delete, grep(pattern = "<", allSkipFiveGrams$nextword))
allSkipFiveGrams_de <- allSkipFiveGrams[-unique(delete), ]
load("allSkipSixGrams_clean_de.RData")
bigrams <- do.call(rbind, strsplit(allSkipSixGrams$ngram, split = " "))
allSkipSixGrams$ngram <- bigrams[, 1]
allSkipSixGrams$nextword <- bigrams[, 2]
delete <- grep(pattern = "^'+", allSkipSixGrams$ngram)
delete <- c(delete, grep(pattern = "^'+", allSkipSixGrams$nextword))
delete <- c(delete, grep(pattern = "<", allSkipSixGrams$nextword))
allSkipSixGrams_de <- allSkipSixGrams[-unique(delete), ]
rm(allSkipFiveGrams, allSkipSixGrams)

# Rename Token data
setnames(allTokens_de, c("nextword", "count"))
setnames(allTokens_en, c("nextword", "count"))

allData <- list(allBigrams_de, allBigrams_en, allBigramsKN_de,
                allBigramsKN_en, allFourgrams_de, allFourgrams_en,
                allFourgramsKN_de, allFourgramsKN_en, allTokens_de,
                allTokens_en, allTrigrams_de, allTrigrams_en,
                allTrigramsKN_de, allTrigramsKN_en,
                allSkipFiveGrams_de, allSkipFiveGrams_en,
                allSkipSixGrams_de, allSkipSixGrams_en)

names(allData) <- c("allBigrams_de", "allBigrams_en", "allBigramsKN_de",
                    "allBigramsKN_en", "allFourgrams_de", "allFourgrams_en",
                    "allFourgramsKN_de", "allFourgramsKN_en", "allTokens_de",
                    "allTokens_en", "allTrigrams_de", "allTrigrams_en",
                    "allTrigramsKN_de", "allTrigramsKN_en",
                    "allSkipFiveGrams_de", "allSkipFiveGrams_en",
                    "allSkipSixGrams_de", "allSkipSixGrams_en")

save(allData, file = "allData.RData")

#----------------------------------------------------------------------------
load("allData.RData")
load("Nextword//data//allData2.RData")
# Further pruning and cleaning (after testing and benchmarking)
allData2 <- allData
allData2 <- lapply(allData2, function(x){
      x[nextword != "<"]
})
# Drop 50% of all n-grams in all tables
allData2 <- lapply(allData2, function(x){
      len <- nrow(x)
      return(x[round(len / 2) : len, ])
})
# allData2 = 330 MB

# Drop all German nextwords that consist of more than one upper case letter
germanIndices <- grep("_de", names(allData2))
allData2[germanIndices] <- lapply(allData2[germanIndices], function(x){
      if(length(x$nextword) <= 0) warning("nextwords not found")
      delete <- grep("[A-Z]{2,}", x$nextword)
      return(x[-delete, ])
})
# about 1MB less

# Quantiles of the counts, for confidence score
minmax <- lapply(allData2, function(x){
      data.table(mininum = min(x$count),
                 maximum = max(x$count))
})

predictNextword_en <- function(input = NULL, ngramData, useKN = T, nPred = 3){
      if (is.null(input)) return(NULL)
      confidence <- NA
      input <- tolower(input)
      splitInput <- unlist(strsplit(input, split = " "))
      # Restrict to trigram input
      if (length(splitInput) > 2){
            input2 <- paste(tail(splitInput, 3), collapse = " ")
      } else {
            input2 <- paste(splitInput, collapse = " ")
      }
      if (useKN){
            fourgramPred <- rev(tail(ngramData$allFourgramsKN_en[ngram == input2]$nextword,
                                     nPred))
            if(length(fourgramPred) > 0 & is.na(confidence)){
                  count <- tail(ngramData$allFourgramsKN_en[ngram == input2]$count, 1)
                  confidence <- quantile((count - minmax$allFourgramsKN_en$mininum) /
                                               (minmax$allFourgramsKN_en$maximum -
                                                      minmax$allFourgramsKN_en$mininum) , 1)
            }
      } else {
            fourgramPred <- rev(tail(ngramData$allFourgrams_en[ngram == input2]$nextword,
                                     nPred))
            if(length(fourgramPred) > 0 & is.na(confidence)){
                  count <- tail(ngramData$allFourgrams_en[ngram == input2]$count, 1)
                  confidence <- quantile((count - minmax$allFourgrams_en$mininum) /
                                               (minmax$allFourgrams_en$maximum -
                                                      minmax$allFourgrams_en$mininum) , 1)
            }
      }
      if (length(fourgramPred) >= nPred){
            return(data.table(pred = fourgramPred, confidence = confidence))
      } else {
            predComb <- fourgramPred
            # Input is a bigram or smaller or no fourgrampred was found
            if (length(splitInput) >= 2){
                  input2 <- paste(tail(splitInput, 2), collapse = " ")
            } else {
                  input2 <- paste(splitInput, collapse = " ")
            }
            if (useKN){
                  trigramPred <- rev(ngramData$allTrigramsKN_en[ngram == input2]$nextword)
                  if(length(trigramPred) > 0 & is.na(confidence)){
                        count <- tail(ngramData$allTrigramsKN_en[ngram == input2]$count, 1)
                        confidence <- quantile((count - minmax$allTrigramsKN_en$mininum) /
                                                     (minmax$allTrigramsKN_en$maximum -
                                                            minmax$allTrigramsKN_en$mininum) , 1)
                  }
            } else {
                  trigramPred <- rev(ngramData$allTrigrams_en[ngram == input2]$nextword)
                  if(length(trigramPred) > 0 & is.na(confidence)){
                        count <- tail(ngramData$allTrigrams_en[ngram == input2]$count, 1)
                        confidence <- quantile((count - minmax$allTrigrams_en$mininum) /
                                                     (minmax$allTrigrams_en$maximum -
                                                            minmax$allTrigrams_en$mininum) , 1)
                  }
            }
            predComb <- c(predComb, trigramPred)
            if (length(unique(predComb)) >= nPred){
                  return(data.table(pred = unique(predComb)[1:nPred],
                                    confidence = confidence))
            } else {
                  # Input is a unigram or no trigrampred was found
                  input2 <- paste(tail(splitInput, 1), collapse = " ")
                  if (useKN){
                        bigramPred <- rev(ngramData$allBigramsKN_en[ngram == input2]$nextword)
                        if(length(bigramPred) > 0 & is.na(confidence)){
                              count <- tail(ngramData$allBigramsKN_en[ngram == input2]$count, 1)
                              confidence <- quantile((count - minmax$allBigramsKN_en$mininum) /
                                                           (minmax$allBigramsKN_en$maximum -
                                                                  minmax$allBigramsKN_en$mininum) , 1)
                        }
                  } else {
                        bigramPred <- rev(ngramData$allBigrams_en[ngram == input2]$nextword)
                        if(length(bigramPred) > 0 & is.na(confidence)){
                              count <- tail(ngramData$allBigrams_en[ngram == input2]$count, 1)
                              confidence <- quantile((count - minmax$allBigrams_en$mininum) /
                                                           (minmax$allBigrams_en$maximum -
                                                                  minmax$allBigrams_en$mininum) , 1)
                        }
                  }
                  predComb <- c(predComb, bigramPred)
                  if (length(unique(predComb)) >= nPred){
                        return(data.table(pred = unique(predComb)[1:nPred],
                                          confidence = confidence))
                  } else {
                        # Input is an unknown n-gram / token
                        input2 <- paste(tail(splitInput, 1), collapse = " ")
                        skipfivegramPred <- rev(ngramData$allSkipFiveGrams_en[ngram == input2]$nextword)
                        if(length(skipfivegramPred) > 0 & is.na(confidence)){
                              count <- tail(ngramData$allSkipFiveGrams_en[ngram == input2]$count, 1)
                              confidence <- quantile((count - minmax$allSkipFiveGrams_en$mininum) /
                                                           (minmax$allSkipFiveGrams_en$maximum -
                                                                  minmax$allSkipFiveGrams_en$mininum) , 1)
                        }
                        predComb <- c(predComb, skipfivegramPred)
                        if (length(unique(predComb)) >= nPred){
                              return(data.table(pred = unique(predComb)[1:nPred],
                                                confidence = confidence))
                        } else {
                              # Try skip-6-grams
                              skipsixgramPred <- rev(ngramData$allSkipSixGrams_en[ngram == input2]$nextword)
                              if(length(skipsixgramPred) > 0 & is.na(confidence)){
                                    count <- tail(ngramData$allSkipSixGrams_en[ngram == input2]$count, 1)
                                    confidence <- quantile((count - minmax$allSkipSixGrams_en$mininum) /
                                                                 (minmax$allSkipSixGrams_en$maximum -
                                                                        minmax$allSkipSixGrams_en$mininum) , 1)
                              }
                              predComb <- c(predComb, skipsixgramPred)
                              if (length(unique(predComb)) >= nPred){
                                    return(data.table(pred = unique(predComb)[1:nPred],
                                                      confidence = confidence))
                              } else {
                                    # No matching (skip) n-grams found
                                    tokenPred <- rev(tail(ngramData$allTokens_en$nextword, nPred))
                                    predComb <- c(predComb, tokenPred)
                                    return(data.table(pred = unique(predComb)[1:nPred],
                                                      confidence = confidence))
                              }
                        }
                  }
            }
      }
}

predWrapper <- function(x){
      predictNextword_en(input = x, ngramData = allData2, useKN = T, nPred = 3)$pred
}

predictNextword_en(input = "Adam Sandler is", ngramData = allData2, useKN = T)


predictNextword_de <- function(input = NULL, ngramData, useKN = T, nPred = 3){
      if (is.null(input)) return(NULL)
      confidence <- NA
      splitInput <- unlist(strsplit(input, split = " "))
      # Restrict to trigram input
      if (length(splitInput) > 2){
            input2 <- paste(tail(splitInput, 3), collapse = " ")
      } else {
            input2 <- paste(splitInput, collapse = " ")
      }
      if (useKN){
            fourgramPred <- rev(tail(ngramData$allFourgramsKN_de[ngram == input2]$nextword,
                                     nPred))
            if(length(fourgramPred) > 0 & is.na(confidence)){
                  count <- tail(ngramData$allFourgramsKN_de[ngram == input2]$count, 1)
                  confidence <- quantile((count - minmax$allFourgramsKN_de$mininum) /
                                               (minmax$allFourgramsKN_de$maximum -
                                                      minmax$allFourgramsKN_de$mininum) , 1)
            }
      } else {
            fourgramPred <- rev(tail(ngramData$allFourgrams_de[ngram == input2]$nextword,
                                     nPred))
            if(length(fourgramPred) > 0 & is.na(confidence)){
                  count <- tail(ngramData$allFourgrams_de[ngram == input2]$count, 1)
                  confidence <- quantile((count - minmax$allFourgrams_de$mininum) /
                                               (minmax$allFourgrams_de$maximum -
                                                      minmax$allFourgrams_de$mininum) , 1)
            }
      }
      if (length(fourgramPred) >= nPred){
            return(data.table(pred = fourgramPred, confidence = confidence))
      } else {
            predComb <- fourgramPred
            # Input is a bigram or smaller or no fourgrampred was found
            if (length(splitInput) >= 2){
                  input2 <- paste(tail(splitInput, 2), collapse = " ")
            } else {
                  input2 <- paste(splitInput, collapse = " ")
            }
            if (useKN){
                  trigramPred <- rev(ngramData$allTrigramsKN_de[ngram == input2]$nextword)
                  if(length(trigramPred) > 0 & is.na(confidence)){
                        count <- tail(ngramData$allTrigramsKN_de[ngram == input2]$count, 1)
                        confidence <- quantile((count - minmax$allTrigramsKN_de$mininum) /
                                                     (minmax$allTrigramsKN_de$maximum -
                                                            minmax$allTrigramsKN_de$mininum) , 1)
                  }
            } else {
                  trigramPred <- rev(ngramData$allTrigrams_de[ngram == input2]$nextword)
                  if(length(trigramPred) > 0 & is.na(confidence)){
                        count <- tail(ngramData$allTrigrams_de[ngram == input2]$count, 1)
                        confidence <- quantile((count - minmax$allTrigrams_de$mininum) /
                                                     (minmax$allTrigrams_de$maximum -
                                                            minmax$allTrigrams_de$mininum) , 1)
                  }
            }
            predComb <- c(predComb, trigramPred)
            if (length(unique(predComb)) >= nPred){
                  return(data.table(pred = unique(predComb)[1:nPred],
                                    confidence = confidence))
            } else {
                  # Input is a unigram or no trigrampred was found
                  input2 <- paste(tail(splitInput, 1), collapse = " ")
                  if (useKN){
                        bigramPred <- rev(ngramData$allBigramsKN_de[ngram == input2]$nextword)
                        if(length(bigramPred) > 0 & is.na(confidence)){
                              count <- tail(ngramData$allBigramsKN_de[ngram == input2]$count, 1)
                              confidence <- quantile((count - minmax$allBigramsKN_de$mininum) /
                                                           (minmax$allBigramsKN_de$maximum -
                                                                  minmax$allBigramsKN_de$mininum) , 1)
                        }
                  } else {
                        bigramPred <- rev(ngramData$allBigrams_de[ngram == input2]$nextword)
                        if(length(bigramPred) > 0 & is.na(confidence)){
                              count <- tail(ngramData$allBigrams_de[ngram == input2]$count, 1)
                              confidence <- quantile((count - minmax$allBigrams_de$mininum) /
                                                           (minmax$allBigrams_de$maximum -
                                                                  minmax$allBigrams_de$mininum) , 1)
                        }
                  }
                  predComb <- c(predComb, bigramPred)
                  if (length(unique(predComb)) >= nPred){
                        return(data.table(pred = unique(predComb)[1:nPred],
                                          confidence = confidence))
                  } else {
                        # Input is an unknown n-gram / token
                        input2 <- paste(tail(splitInput, 1), collapse = " ")
                        skipfivegramPred <- rev(ngramData$allSkipFiveGrams_de[ngram == input2]$nextword)
                        if(length(skipfivegramPred) > 0 & is.na(confidence)){
                              count <- tail(ngramData$allSkipFiveGrams_de[ngram == input2]$count, 1)
                              confidence <- quantile((count - minmax$allSkipFiveGrams_de$mininum) /
                                                           (minmax$allSkipFiveGrams_de$maximum -
                                                                  minmax$allSkipFiveGrams_de$mininum) , 1)
                        }
                        predComb <- c(predComb, skipfivegramPred)
                        if (length(unique(predComb)) >= nPred){
                              return(data.table(pred = unique(predComb)[1:nPred],
                                                confidence = confidence))
                        } else {
                              # Try skip-6-grams
                              skipsixgramPred <- rev(ngramData$allSkipSixGrams_de[ngram == input2]$nextword)
                              if(length(skipsixgramPred) > 0 & is.na(confidence)){
                                    count <- tail(ngramData$allSkipSixGrams_de[ngram == input2]$count, 1)
                                    confidence <- quantile((count - minmax$allSkipSixGrams_de$mininum) /
                                                                 (minmax$allSkipSixGrams_de$maximum -
                                                                        minmax$allSkipSixGrams_de$mininum) , 1)
                              }
                              predComb <- c(predComb, skipsixgramPred)
                              if (length(unique(predComb)) >= nPred){
                                    return(data.table(pred = unique(predComb)[1:nPred],
                                                      confidence = confidence))
                              } else {
                                    # No matching (skip) n-grams found
                                    tokenPred <- rev(tail(ngramData$allTokens_de$nextword, nPred))
                                    predComb <- c(predComb, tokenPred)
                                    #                                     print("tokenPred")
                                    return(data.table(pred = unique(predComb)[1:nPred],
                                                      confidence = confidence))
                              }
                        }
                  }
            }
      }
}

# Demo
suppressWarnings(
      predictNextword_de(input = "Die Tagesordnung",
                         ngramData = allData2, useKN = T)
)

