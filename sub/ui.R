library(shiny)
shinyUI(pageWithSidebar(
      headerPanel("Price summary of recent auctions on ebay.com"),
      
      sidebarPanel(
            helpText("Search term:"),
            textInput("searchterm", " ", ""),
            
            helpText("Format:"),
            radioButtons("type", " ",
                         list("All" = "all",
                              "Buy it now" = "buyitnow",
                              "Auction" = "auction"), selected="all"),
            
            helpText("Condition:"),
            radioButtons("condition", " ",
                         list("All" = "all",
                              "New" = "new",
                              "Used" = "used"), selected="all"),
            
            sliderInput("maxarticles", 
                        "Maximum number of auctions that should be loaded:", 
                        min=50, max=400, value=100, step=50),
            
            actionButton("go", "Go!"),
            
            br(),
            br(),
            br(),
            
            sliderInput("bars", "Number of bars in the histogram (target value):", 
                        min=5, max=100, value=15),
            br(),
            
            helpText("Statistics:"),
            textOutput("text1"),
            
            textOutput("text3"),
            
            textOutput("text5"),
            
            textOutput("text7"),
            br(),
            
            textOutput("n"),
            # textOutput("daterange"),
            textOutput("text11"),
            textOutput("generatedurl")
      ),
      
      mainPanel(
            plotOutput("ebay_histogram", width="100%", height=700)
      )
))