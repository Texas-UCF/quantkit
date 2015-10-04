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
                                  textInput("components", "Components (Comma separated)"),
                                  sliderInput("pval", "p-value", 0, 1, value=0.05),
                                  dateInput("startDate", "From", value = Sys.Date() - 365),
                                  dateInput("endDate", "To", value = Sys.Date()),
                                  numericInput("hedgeshares", label = "Number of Shares", value = 100),
                                  submitButton("Filter")),
                      mainPanel(tableOutput("regressionTable"),
                                showOutput("timeseries", "Highcharts")
                      )
                     
                      ),
  
            tabPanel("Special Movements"
                     
                     ),
            tabPanel('Correlation Analysis')
  )
  
  
  
  
))