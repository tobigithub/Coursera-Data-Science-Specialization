# Function to return a data.table of n-grams and counts
# Removes punctuation and whitespace, converts all characters to lower
# Can replace sentence ends with </s>
# Converts all documents to a specified encoding
# Input is a character vector of the documents

makeNgrams <- function(x, ngram = 2L, markSentences = F,
                       encoding = "ASCII", skip = F, convertToLower = T){
      startFunction <- Sys.time()
      require(tau)
      source('~/WualaDrive/asc90698583/Wuala Sync/Diverses/Coursera/DATA SCIENCE SPECIALIZATION/10 Capstone/R/getSkipNgrams.R')
      punct <- '[]\\?!\"#$%&(){}+*:;,._`|~\\[\\=\\@\\^-]'
      segmentsEnds <- seq(1e3, length(x) + 1e3, 1e3)
      segments <- segmentsEnds - 999
      message(paste("Number of iterations:", length(segments)))
      for (i in seq_along(segments)){
            start <- Sys.time()
            ngrams <- na.omit(x[segments[i]:segmentsEnds[i]])
            ngrams <- iconv(ngrams, to = encoding, sub="")
            # Remove hashtags
            ngrams <- gsub("#[[:alpha:]]*", "", ngrams)
            # Replace exclamation marks and question marks by a single period
            ngrams <- gsub("\\!|\\?", ".", ngrams)
            # Set markers for sentence ends and remove punctuation -----------
            if (markSentences){
                  ngrams <- gsub("\\.+", " </s> ", ngrams)
            } else ngrams <- gsub("\\.+", "", ngrams)
            ngrams <- gsub(punct, "", ngrams)
            # ---------------------------------------------------------------
            # Remove numbers
            ngrams <- gsub("[0-9]", "", ngrams)
            if (convertToLower){
                  ngrams <- tolower(ngrams)
            }
            if (skip){
                  ngrams <- getSkipNgrams(text = ngrams, n = ngram)
            } else {
            ngrams <- textcnt(ngrams, tolower = F,
                              method = "string",
                              n = ngram,
                              decreasing = T,
                                     split = "[[:space:][:digit:]]+")
            # Combine
            ngrams <- data.table("count" = as.numeric(ngrams),
                                        "ngram" = names(ngrams))
            }
            if (i == 1){
                  allNgrams <- ngrams
            } else {
                  allNgrams <- rbind.fill(allNgrams, ngrams)
                  allNgrams <- data.table(allNgrams)
                  allNgrams <- allNgrams[, lapply(.SD, sum), by = ngram]
            }

            # At 1/3 and 2/3 of the complete run delete all n-grams with count 1
            if (i == round(length(segments) * (1/3)) |
                      i == round(length(segments) * (2/3))){
                  Nrare <- nrow(allNgrams[count == 1])
                  message(paste(Nrare, "rare n-grams removed"))
                  allNgrams <- allNgrams[count > 1]
                  gc()
            }

            print(i)
            print(Sys.time() - start)
      }
      message("Finished after:")
      print(Sys.time() - startFunction)
      allNgrams <- allNgrams[count > 1]
      return(allNgrams)
}
