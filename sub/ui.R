library(shiny)
shinyUI(pageWithSidebar(
      h3("Price summary of recent auctions on ebay.com"),
      
      sidebarPanel(
            h4("Search term:"),
            helpText("Input a product you are looking for. You can also use 
                        operators like 'minus' as on the ebay page, for example:
                        'Window -broken'"),
            textInput("searchterm", " ", ""),
            
            h4("Format:"),
            helpText("You can search for only auctions, only articles that are 
                     for immediate sale at a fixed price or both."),
            radioButtons("type", " ",
                         list("All" = "all",
                              "Buy it now" = "buyitnow",
                              "Auction" = "auction"), selected="all"),
            
            h4("Condition:"),
            helpText("You can search for new articles, used articles or both"),
            radioButtons("condition", " ",
                         list("All" = "all",
                              "New" = "new",
                              "Used" = "used"), selected="all"),
            
            sliderInput("maxarticles", 
                        "Maximum number of auctions that should be loaded:", 
                        min=50, max=400, value=100, step=50),
            
            h3("Load auction results"),
            actionButton("go", "Go!"),
            
            h4("Statistics"),        
            sliderInput("bars", "Number of bars in the histogram (target value):", 
                        min=5, max=100, value=15),
            br(),
            
            textOutput("text1"),
            
            textOutput("text3"),
            
            textOutput("text5"),
            
            textOutput("text7"),
            br(),
            
            textOutput("n"),
            # textOutput("daterange"),
            textOutput("text11"),
            helpText("You can find the results of your search on the ebay page at:"),
            textOutput("generatedurl")
      ),
      
      mainPanel(
            plotOutput("ebay_histogram", width="100%", height=700)
      )
))