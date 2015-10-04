library(shiny)
library(ggplot2)
library(rCharts)
library(rHighcharts)
library(quantkit)

shinyUI(fluidPage(
  
  navbarPage("Quantkit",
            #Filter Components Tab
            tabPanel("Filter Components", 
                     sidebarPanel(textInput("ticker", "Ticker"),
                                  textInput("components", "Components (Comma separated)", "^GSPC"),
                                  sliderInput("pval", "p-value", 0, 1, value=0.05),
                                  dateInput("startDate", "From", value = Sys.Date() - 365),
                                  dateInput("endDate", "To", value = Sys.Date()),
                                  numericInput("hedgeshares", label = "Number of Shares", value = 100),
                                  radioButtons("charttype", "Output:", 
                                               c("Unfiltered Price",
                                                 "Filtered Price",
                                                 "Unfiltered Returns",
                                                 "Filtered Returns",
                                                 "Unfiltered Return Moments", 
                                                 "Filtered Return Moments",
                                                 "Moving Correlation")),
                                  submitButton("Run"),
                                  br(),
                                  tableOutput("regressionstats")),
                      mainPanel(
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"),
                        showOutput("plot", "Highcharts"))
                      ),
            
            #TODO: Add filter components 
            tabPanel("Special Movements",
                     sidebarPanel(textInput("ticker", "Ticker"),
                                  sliderInput("stddev", "Num. Standard Deviations Moved", 0, 10, value=2),
                                  dateInput("startDate", "From", value = Sys.Date() - 365),
                                  dateInput("endDate", "To", value = Sys.Date()),
                                  radioButtons("event", "Event Type:", 
                                               c("Large Moves",
                                                 "Earnings Moves")),
                                  submitButton("Run")),                                  
                     mainPanel(
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       showOutput("plot", "Highcharts"))
                     
                     ),
            
            tabPanel("Similar Stocks",
                     sidebarPanel(textInput("ticker", "Ticker"),
                                  sliderInput("mcap", "Market Cap % Difference", 0, 1, value=.1),
                                  checkboxGroupInput("limits", "Specifications:", 
                                               c("Sector",
                                                 "Industry")),
                                  submitButton("Run")),                                  
                     mainPanel(
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       textOutput("similar"),
                       tableOutput("correlations"),
                       tableOutput("covariances")
                       )
                     
                     
            ),
            tabPanel("Quick Facts",
                     sidebarPanel(textInput("tickerlist", "Tickers"),
                                  checkboxGroupInput("stats", "Key Statistics:", 
                                                     readLines("keystats.txt")),
                                  submitButton("Run")),
                     mainPanel(
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }")
                       
                     )
                     
                     
            )
            
  )
  
  
  
  
))