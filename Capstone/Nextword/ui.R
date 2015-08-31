
# if (Sys.info()['sysname'] == "Windows"){
#       setwd("W:\\asc90698583\\Wuala Sync\\Diverses\\Coursera\\DATA SCIENCE SPECIALIZATION\\10 Capstone\\R\\")
# } else setwd("/home/khl4v/Wuala Sync/Diverses/Coursera/DATA SCIENCE SPECIALIZATION/10 Capstone/R/")

library(shiny)
shinyUI(navbarPage("Next word prediction",
                   tabPanel("Main app",
                            # shinyUI(
                            fluidPage(
                                  #                                   titlePanel("Predict the next word"),
                                  fluidRow(
                                        column(4,
                                               h2("Start typing..."),
                                               p("Please wait for the confidence indicator
                                                 to load"),
                                               textInput("ngram", label = "Your input
                                                         will be evaluated automatically.",
                                                         value = "")
                                        ),
                                        column(4,
                                               h4("Prediction"),
                                               p("The most probable word is displayed at the top"),
                                               textOutput("pred1", h2),
                                               textOutput("pred2", h4),
                                               textOutput("pred3", h4)
                                        ),
                                        column(4,
                                               h4("Probable next words"),
                                               p("Based on Markov Chain predictions"),
                                               textOutput("nextwords", h2)
                                        )
                                  ),
                                  fluidRow(
                                        column(4,
                                               hr(),
                                               radioButtons("selectLan",
                                                            label = h4("Language"),
                                                            choices = list("English" = "en",
                                                                           "German / Deutsch" = "de")),
                                               radioButtons("selectModel",
                                                            label = h4("Type of model"),
                                                            choices = list("Raw counts with
                                               (Skip-)n-gram-Backoff" = FALSE,
                                                                           "Kneser-Ney-Smoothing with
                                               Skip-n-gram-Backoff" = TRUE))
                                        ),
                                        column(4,
                                               hr(),
                                               h4("Relative confidence"),
                                               p("for the top prediction"),
                                               plotOutput("confidencePlot")
                                        )
                                  )
                            )),

                   # 2. Tab --------------------------------------------------
                   tabPanel("Description of the algorithms",
                            includeMarkdown("technicalNotes.md")
                   ),

#                    # 3. Tab --------------------------------------------------
#                    tabPanel("Further notes and remarks to the grader",
#                             includeMarkdown("personalNote.html")
#                    ),


                   # 4. Tab --------------------------------------------------
                   tabPanel("Benchmarks",
                            includeMarkdown("benchmarks.md")
                   )

#                    # 5. Tab --------------------------------------------------
#                    tabPanel("Milestone Report",
#                             includeMarkdown("Milestone_Report_1_Exploratory_Analysis.md")
#                    )

))
