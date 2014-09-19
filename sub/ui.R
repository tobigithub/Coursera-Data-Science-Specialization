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
                              "Auction" = "auction"), selected="All"),
            
            helpText("Condition:"),
            radioButtons("condition", " ",
                         list("All" = "all",
                              "New" = "new",
                              "Used" = "used"), selected="All"),
            
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
            
            # Geht das?
            #     textOutput(c("text1", "text2")),
            #     textOutput(c("text3", "text4")),
            #     textOutput(c("text5", "text6")),
            #     textOutput(c("text7", "text8")),
            helpText("Statistics:"),
            textOutput("text1"),
            #   textOutput("text2"),
            
            textOutput("text3"),
            #   textOutput("text4"),
            
            textOutput("text5"),
            #   textOutput("text6"),
            
            textOutput("text7"),
            #  textOutput("text8"),
            br(),
            
            textOutput("n"),
            textOutput("daterange"),
            textOutput("text11"),
            textOutput("generatedurl")
      ),
      
      mainPanel(
            plotOutput("ebay_histogram", width="100%", height=700)
      )
))