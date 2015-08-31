# PACKAGES
require(shiny)
require(data.table)
require(rmarkdown)

# LOAD DATA
load("data/minmax.RData")
load("data/allData2.RData")

# FUNCTION DEFINITIONS
predictNextword_en <- function(input = NULL, ngramData = allData2, useKN = T, nPred = 3){
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
            #             print("fourgramPred")
            #                         print(fourgramPred)
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
                  #                   print("trigramPred")
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
                        #                         print("bigramPred")
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
                              #                               print("skipfivegramPred")
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
                                    #                                     print("skipsixgramPred")
                                    return(data.table(pred = unique(predComb)[1:nPred],
                                                      confidence = confidence))
                              } else {
                                    # No matching (skip) n-grams found
                                    tokenPred <- rev(tail(ngramData$allTokens_en$nextword, nPred))
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

predictNextword_de <- function(input = NULL, ngramData = allData2, useKN = T, nPred = 3){
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
            #             print("fourgramPred")
            #                         print(fourgramPred)
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
                  #                   print("trigramPred")
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
                        #                         print("bigramPred")
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
                              #                               print("skipfivegramPred")
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
                                    #                                     print("skipsixgramPred")
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

# SHINY SERVER -----------------------------------------------------
shinyServer(function(input, output){
      text_reactive <- reactive({
            if (nchar(input$ngram) > 0){
                  if (input$selectLan == "en"){
                        predictNextword_en(input = input$ngram,
                                           ngramData = allData2,
                                           useKN = input$selectModel,
                                           nPred = 3)$pred
                  } else {
                        predictNextword_de(input = input$ngram,
                                           ngramData = allData2,
                                           useKN = input$selectModel,
                                           nPred = 3)$pred
                  }
            }
      })

      prob_reactive <- reactive({
            if (nchar(input$ngram) > 0){
                  if (input$selectLan == "en"){
                        predictNextword_en(input = input$ngram,
                                           ngramData = allData2,
                                           useKN = input$selectModel,
                                           nPred = 1)$confidence
                  } else {
                        predictNextword_de(input = input$ngram,
                                           ngramData = allData2,
                                           useKN = input$selectModel,
                                           nPred = 1)$confidence
                  }
            } else {
                  return(0)
            }
      })

      nextwords_reactive <- reactive({
                  if (nchar(input$ngram) > 0){
                        if (input$selectLan == "en"){
                              nextwords <- c()
                              latestWord <- text_reactive()[1]
                              for (i in 1:3){
                                    nextwords[i] <- predictNextword_en(input = unlist(latestWord),
                                                                       ngramData = allData2,
                                                                       useKN = input$selectModel,
                                                                       nPred = 1)
                                    latestWord <- nextwords[i]
                              }
                              nextwords <- paste(nextwords, collapse = " ")
                              return(nextwords)
                        } else {
                              nextwords <- c()
                              latestWord <- text_reactive()[1]
                              for (i in 1:3){
                                    nextwords[i] <- predictNextword_de(input = unlist(latestWord),
                                                                       ngramData = allData2,
                                                                       useKN = input$selectModel,
                                                                       nPred = 1)
                                    latestWord <- nextwords[i]
                              }
                              nextwords <- paste(nextwords, collapse = " ")
                              return(nextwords)
                        }
                  }
            })

      output$pred1 <- renderText({
            text_reactive()[1]
      })
      output$pred2 <- renderText({
            text_reactive()[2]
      })
      output$pred3 <- renderText({
            text_reactive()[3]
      })

      output$nextwords <- renderText({
            nextwords_reactive()
      })

      output$confidencePlot <- renderPlot({
            prob <- sqrt(prob_reactive())
            dat <- as.table(matrix(data = c(prob, 1-prob), nrow = 2))
            colnames(dat) <- NULL
            barplot(dat, horiz=TRUE, col=c("darkblue","grey"))
      }, height = 200, width = 300)
})
