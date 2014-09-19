library(shiny)
library(XML)
library(RCurl)

# SCRAPING FUNCTION
scrapeEbay.de <- function(search = NA, cond = "all", auctiontype = "all", maxn = 100){
  
  if (is.na(search)) stop("Please enter a search term or a URL.")
  #if (is.na(search) == FALSE & is.na(url) == FALSE) stop("Please enter a either search term or a URL, not both.")
  
  # How many pages on ebay.de?
  if (maxn > 250) maxn <- 250
  intervals <- c(51, 101, 151, 201, 251, 301, 351, 401)
  pages <- findInterval(maxn, intervals) + 1
  
  prices <- NA
  alldates <- NA
  dates <- NA
  for (p in 1:pages){
    if (p>1) Sys.sleep(0.5)
    # URL or search term?
    if (length(grep(pattern="bay.de/", x = search)) == 0){ # if True not a URL
      
      # Create URL for search:
      # cond used or new
      # auctiontype auction or buyitnow
      # format search for use in the URL:
      searchterm <- gsub(" ", "+", search)
      
      url <- paste0("http://www.ebay.de/sch/i.html?_from=R40",
                    if (cond=="new") "&LH_ItemCondition=3" else "",
                    if (cond=="used") "&LH_ItemCondition=4" else "",
                    if (auctiontype=="auction") "&LH_Auction=1" else "",
                    if (auctiontype=="buyitnow") "&LH_BIN=1" else "",
                    "&_nkw=", searchterm,
                    "&LH_Complete=1&LH_Sold=1&rt=nc")
    }
    
    # Save URL for output:
    if (p == 1) generatedurl <- url
    
    # Select page:
    if (p > 1){
      url <- paste0(url, "&_pgn=", p, "&skc=", 50 * p - 50)
    }
    
    # Articles from international sellers? If yes, exclude those. 
    urlcontent <- getURLContent(url)
    if (length(grep("Artikel von internationalen", urlcontent)) > 0){
      urlcontent <- strsplit(x=urlcontent , split="Artikel von internationalen")
      # All relevant auctions are in urlcontent[[1]][1]
      ebay <- readHTMLTable(urlcontent[[1]][1])
    } else{
      if (p == 1) ebay <- readHTMLTable(url)
      if (p > 1){
        ebay <- readHTMLTable(urlcontent)
      }
    }
    
    results <- rep(NA, times=length(ebay)-5)
    if (p == 1 & length(results) <= 0) stop("No search results :-(")
    if (length(results) <= 0) break # No more results
    
    for (i in 1:length(results)){
      # List entries (auction results) start at [[3]]
      temp <- ebay[[i+2]]
      
      # Datum
      datestr <- as.character(temp$V3)
      m_datestr <- regexec("[0-9]*..[A-Z][a-z]*", datestr)
      datestr <- regmatches(datestr, m_datestr)
      dates[i] <- as.character(datestr)
      
      # Price:
      temp <- as.character(temp$V4)
      m_price <- regexec("EUR.[0-9]*.[0-9]*", temp)
      # character vector:
      # e.g. "EUR 212,52"
      Price <- regmatches(temp, m_price)
      Price <- substr(x=Price, start=5, stop=nchar(Price))
      Price <- gsub(pattern=",", replacement=".", x=Price)
      Price <- as.numeric(Price)
      
      # Shipping:
      m_shipping <- regexec("[0-9]*.[0-9]*.Versand", temp)
      # character vector:
      # e.g. "5,90 Versand"
      Shipping <- as.character(regmatches(temp, m_shipping))
      # Only numerical value:
      m_shipping2 <- regexec("[0-9]*.[0-9]*", Shipping)
      Shipping2 <- as.character(regmatches(Shipping, m_shipping2))
      Shipping2 <- gsub(pattern=",", replacement=".", x=Shipping2)
      Shipping2 <- suppressWarnings(as.numeric(Shipping2))
      
      # If shipping is free, Shipping2 will be NA
      if (is.na(Shipping2)) Shipping2 <- 0
      
      # Price + Shipping:
      results[i] <- Price + Shipping2
    }
    
    # Ebay may (!) accept page numbers even if that page does not exist
    # It returns the results of the previous page. Check:
    if (p == 1){
      prevresults <- results
    } else{
      if (all(results == prevresults)) break
      prevresults <- results
    }
    
    prices <- c(prices, results)
    alldates <- c(alldates, dates)
  }
  prices <- na.omit(prices)
  alldates <- na.omit(alldates)
  resultlist <- list(prices, alldates, generatedurl)
  names(resultlist) <- c("Prices", "Date", "URL")
  return(resultlist)
}


# SHINY SERVER:
shinyServer(function(input, output) { 
  
  prices_dummylist <- list(0, c(" ", " "), " ")
  names(prices_dummylist) <- c("Prices", "Date", "URL")
  
  prices_reactive <- reactive({
    if (input$go == 0) return(prices_dummylist)
    isolate(
      scrapeEbay.de(search = as.character(input$searchterm), 
                    cond = as.character(input$condition),
                    auctiontype = as.character(input$type),
                    maxn = input$maxarticles)
    )
  })
  
  
  # RENDER PLOT:
  output$ebay_histogram <- renderPlot(function() {
    hist(x=prices_reactive()$Prices, col = "blue", breaks = as.numeric(input$bars), 
         main="Histogram", xlab="Price including shipping", ylab="Amount")
  })
  
  # TEXT OUTPUT
  #   output$text1 <- renderText({ 
  #     "Mininum:"
  #   })
  output$text1 <- renderText({ 
    paste("Minimum:", as.numeric(summary(object=prices_reactive()$Prices)[1]))
  })
  
  #   output$text3 <- renderText({ 
  #     "Median:"
  #   })
  output$text3 <- renderText({ 
    paste("Median:", as.numeric(summary(object=prices_reactive()$Prices)[3]))
  })
  
  #   output$text5 <- renderText({ 
  #     "Mittelwert:"
  #   })
  output$text5 <- renderText({ 
    paste("Mean:", as.numeric(summary(object=prices_reactive()$Prices)[4]))
  })
  
  #   output$text7 <- renderText({ 
  #     "Maximum:"
  #   })
  output$text7 <- renderText({ 
    paste("Maximum:", as.numeric(summary(object=prices_reactive()$Prices)[6]))
  })
  
  #   Number of auctions
  #   output$text9 <- renderText({ 
  #     "Gefundene Auktionen:"
  #   })
  output$n <- renderText({ 
    paste("Auctions:", length(prices_reactive()$Prices))
  })
  
  # Date Range
  #   output$text10 <- renderText({ 
  #     "Datumsbereich:"
  #   })
  output$daterange <- renderText({ 
    paste0("Date range: ", prices_reactive()$Date[length(prices_reactive()$Date)],
           " - ",
           prices_reactive()$Date[1])
  })
  
  # Generated URL
  output$text11 <- renderText({ 
    "Generated URL:"
  })
  output$generatedurl <- renderText({ 
    prices_reactive()$URL
  })
})
