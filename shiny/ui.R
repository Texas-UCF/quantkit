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
                     sidebarPanel(textInput("specialticker", "Ticker"),
                                  textInput("movementcomponents", "Components (Comma separated)", "^GSPC"),
                                  sliderInput("stddev", "Num. Standard Deviations Moved", 0, 10, value=2),
                                  dateInput("startDate2", "From", value = Sys.Date() - 365),
                                  dateInput("endDate2", "To", value = Sys.Date()),
                                  radioButtons("event", "Event Type:", 
                                               c("Large Moves",
                                                 "Earnings Moves")),
                                  checkboxInput("filtermovement", "Filter Components"),
                                  submitButton("Run")),                                  
                     mainPanel(
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       showOutput("plots", "Highcharts"))
                     
                     ),
            
            tabPanel("Similar Stocks",
                     sidebarPanel(textInput("similarticker", "Ticker"),
                                  sliderInput("mcap", "Market Cap % Difference", 0, 1, value=.1),
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
                       tableOutput("correlations"),
                       h3("Covariance Matrix"),
                       tableOutput("covariances")
                       )
                     
                     
            ),
            
            tabPanel("Quick Facts",
                     sidebarPanel(textInput("tickerlist", "Tickers (Comma Separated)"),
                                  
                                  #TODO: Implement Select All
                                  checkboxGroupInput("quickstats", "Key Statistics:", 
                                                     readLines("keystats.txt")),
                                  submitButton("Run")),
                     mainPanel(
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       dataTableOutput("factsTable")
                       
                     )
                     
                     
            )
            
  )
  
  
  
  
))
