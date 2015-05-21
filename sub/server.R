library(shiny)
library(XML)
library(RCurl)

# SCRAPING FUNCTION
scrapeEbay <- function(search = NA, cond = "all", auctiontype = "all", maxn = 100){
      
      if (is.na(search)) stop("Please enter a search term.")
      
      # How many pages?
      if (maxn > 250) maxn <- 250
      intervals <- c(51, 101, 151, 201, 251, 301, 351, 401)
      pages <- findInterval(maxn, intervals) + 1 # How many pages have to be loaded?
      
      prices <- NA
      alldates <- NA
      dates <- NA
      for (p in 1:pages){
            # Random waiting time
            if (p>1) Sys.sleep(abs(rnorm(1, mean = 0.5, sd = 0.5))) 
            
            # Create URL for search:
            # cond: used or new
            # auctiontype: auction or buyitnow
            # format search for use in the URL, replace whitespace by plus:
            searchterm <- gsub(" ", "+", search)
            
            url <- paste0("http://www.ebay.com/sch/i.html?_from=R40",
                          if (cond=="new") "&LH_ItemCondition=3" else "",
                          if (cond=="used") "&LH_ItemCondition=4" else "",
                          if (auctiontype=="auction") "&LH_Auction=1" else "",
                          if (auctiontype=="buyitnow") "&LH_BIN=1" else "",
                          "&_nkw=", searchterm,
                          "&LH_Complete=1&LH_Sold=1&rt=nc")
            
            
            # Save URL for output:
            if (p == 1) generatedurl <- url
            
            # Select page:
            if (p > 1){
                  url <- paste0(url, "&_pgn=", p, "&skc=", 50 * p - 50)
            }
            
            # Articles from international sellers? If yes, exclude those. 
            # These are suggestions based on the search and should not be included
            urlcontent <- getURLContent(url)
            if (length(grep("international sellers", urlcontent)) > 0){
                  urlcontent <- strsplit(x=urlcontent , 
                                         split="international sellers")
                  # All relevant auctions are in urlcontent[[1]][1]
                  urlcontent <- urlcontent[[1]][1]
            }
            
            # Related articles? If yes, exclude those. 
            # These are suggestions based on the search and should not be included
            if (length(grep("items related to", urlcontent)) > 0){
                  urlcontent <- strsplit(x=urlcontent , 
                                         split="items related to")
                  # All relevant auctions are in urlcontent[[1]][1]
                  urlcontent <- urlcontent[[1]][1]
            }
            
            # Grep prices and store as vector
            temp <- strsplit(urlcontent, split = "/")
            temp <- temp[[1]]
            price_strings <- temp[grep(pattern = "\\$[0-9]+\\.[0-9]{2}", x = temp)]
            Pricevec <- NA
            for (i in seq_along(price_strings)){
                  m_price <- regexec("\\$[0-9]+\\.[0-9]{2}", price_strings[i])
                  # character vector:
                  # e.g. "$212.52"
                  Pricetemp <- regmatches(price_strings[i], m_price)
                  Pricetemp <- unlist(Pricetemp)
                  Pricetemp <- gsub(pattern="\\$", replacement="", x=Pricetemp)
                  Pricevec[i] <- as.numeric(Pricetemp); rm(Pricetemp)
            }
            
            if (p == 1 & is.na(Pricevec[1])) stop("No search results :-(")
            if (p > 1 & is.na(Pricevec[1])) break() # No more results            
            
            
            #                   # Datum
            #                   datestr <- as.character(temp$V3)
            #                   m_datestr <- regexec("[0-9]*..[A-Z][a-z]*", datestr)
            #                   datestr <- regmatches(datestr, m_datestr)
            #                   dates[i] <- as.character(datestr)
            
            
            
            # Ebay may (!) accept page numbers even if that page does not exist
            # It returns the results of the previous page. Check:
            if (p == 1){
                  prevresults <- Pricevec
            } else{
                  if (all(Pricevec == prevresults)) break
                  prevresults <- Pricevec
            }
            
            prices <- c(prices, Pricevec)
            # alldates <- c(alldates, dates)
      }
      prices <- na.omit(prices)
      # alldates <- na.omit(alldates)
      resultlist <- list(prices, 
                         # alldates, 
                         generatedurl)
      names(resultlist) <- c("Prices",
                             # "Date", 
                             "URL")
      return(resultlist)
}


# SHINY SERVER:
shinyServer(function(input, output) { 
      
      prices_dummylist <- list(0, c(" ", " "), " ")
      names(prices_dummylist) <- c("Prices",
                                   # "Date",
                                   "URL")
      
      prices_reactive <- reactive({
            if (input$go == 0) return(prices_dummylist)
            isolate(
                  scrapeEbay(search = as.character(input$searchterm), 
                                cond = as.character(input$condition),
                                auctiontype = as.character(input$type),
                                maxn = input$maxarticles)
            )
      })
      
      
      # RENDER PLOT:
      output$ebay_histogram <- renderPlot(function() {
            hist(x=prices_reactive()$Prices, col = "blue", breaks = as.numeric(input$bars), 
                 main="Histogram", xlab="Price", ylab="Amount")
      })
      
      # TEXT OUTPUT
      output$text1 <- renderText({ 
            paste("Minimum:", as.numeric(summary(object=prices_reactive()$Prices)[1]))
      })

      output$text3 <- renderText({ 
            paste("Median:", as.numeric(summary(object=prices_reactive()$Prices)[3]))
      })
      
      output$text5 <- renderText({ 
            paste("Mean:", as.numeric(summary(object=prices_reactive()$Prices)[4]))
      })
      
      output$text7 <- renderText({ 
            paste("Maximum:", as.numeric(summary(object=prices_reactive()$Prices)[6]))
      })
      
      #   Number of auctions
      output$n <- renderText({ 
            paste("Auctions:", length(prices_reactive()$Prices))
      })
      
      # Date Range
#       output$daterange <- renderText({ 
#             paste0("Date range: ", prices_reactive()$Date[length(prices_reactive()$Date)],
#                    " - ",
#                    prices_reactive()$Date[1])
#       })
      
      # Generated URL
      output$text11 <- renderText({ 
            "Generated URL:"
      })
      output$generatedurl <- renderText({ 
            prices_reactive()$URL
      })
})
