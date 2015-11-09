library(shiny)
library(ggplot2)
library(rCharts)
library(rHighcharts)
library(quantkit)
library(markdown)

shinyUI(fluidPage(

  navbarPage("Quantkit",
            #Help Tab
            tabPanel("About",
                      includeMarkdown("about_me.md")),
            #Filter Components Tab
            tabPanel("Filter Components",
                     sidebarPanel(textInput("ticker", "Ticker"),
                                  textInput("components", "Components (Comma separated)", "^GSPC"),
                                  sliderInput("pval", "p-value", 0, 1, value=0.05),
                                  dateInput("startDate", "From", value = Sys.Date() - 365),
                                  dateInput("endDate", "To", value = Sys.Date()),
                                  numericInput("hedgeshares", label = "Number of Shares", value = 100),
                                  radioButtons("charttype", "Output:",
                                               c("Unfiltered Price (Stock Price)",
                                                 "Filtered Price",
                                                 "Unfiltered Returns",
                                                 "Filtered Returns",
                                                 "Unfiltered Return Moments",
                                                 "Filtered Return Moments")),
                                  submitButton("Run"),
                                  br()),
                      mainPanel(
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"),
                        showOutput("plot", "highcharts")),
                        tableOutput("regressionstats"),
                        tableOutput("retMoments"),
                        plotOutput("histPlot")
                      ),

            #TODO: Add filter components
            tabPanel("Special Movements",
                     sidebarPanel(textInput("specialticker", "Ticker"),
#                                  checkboxInput("filtermovement", "Filter Components"),
#                                  uiOutput("componentUI"),
                                  sliderInput("stddev", "Num. Standard Deviations Moved", 0, 5, value=2),
                                  dateInput("startDate2", "From", value = Sys.Date() - 365),
                                  dateInput("endDate2", "To", value = Sys.Date()),
                                  numericInput("window", "Plot Date Window Size", 7),
                                  radioButtons("event", "Event Type:",
                                               c("Large Moves",
                                                 "Earnings Moves")),
                                  submitButton("Run")),
                     mainPanel(
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       dataTableOutput("summarymovement"),
                       uiOutput("plots"))

                     ),

            tabPanel("Similar Stocks",
                     sidebarPanel(textInput("similarticker", "Ticker"),
                                  sliderInput("mcap", "Market Cap % Difference", 0.1, 100, value=10),
                                  sliderInput("regcutoff", "Absolute Correlation Coefficient Cutoff", 0, 1, value=.8),
                                  checkboxInput("sector", "Sector", T),
                                  checkboxInput("industry", "Industry"),
                                  submitButton("Run")),
                     mainPanel(
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       h3("Weak Similarities"),
                       dataTableOutput("weaksim"),
                       h3("Strong Similarities"),
                       dataTableOutput("strongsim"),
                       br(),
                       h3("Correlation Matrix"),
                       plotOutput("correlations"),
                       h3("Covariance Matrix"),
                       tableOutput("covariances")
                       )


            ),

            tabPanel("Quick Facts",
                     sidebarPanel(textInput("tickerlist", "Tickers (Comma Separated)"),
                                  checkboxGroupInput("quickstats", "Key Statistics:",
                                                     choices),
                                  submitButton("Run")),
                     mainPanel(
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       dataTableOutput("factsTable")

                     )
            ),
            
            tabPanel("Pairs Trading",
                     sidebarPanel(textInput("pairsTicker1", "Ticker 1"),
                                  textInput("pairsTicker2", "Ticker 2"),
                                  numericInput("pairs_cap", label = "Starting Capital", value = 10000),
                                  sliderInput("pairsThreshold", label = "Num. SDs Threshold", .1, 10, value = 1.5),
                                  dateInput("startDateTrain", "Training Start Date", value = Sys.Date() - 365*2),
                                  dateInput("endDateTrain", "Training End Date", value = Sys.Date() - 365),
                                  dateInput("startDateTest", "Testing Start Date", value = Sys.Date() - 365),
                                  dateInput("endDateTest", "Testing End Date", value = Sys.Date()),
                                  submitButton("Run")
                                  ),
                     mainPanel(
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       showOutput("pairsplot", "highcharts","tradedates")
                     )
           )
  )




))
