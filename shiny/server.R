library(shiny)
library(stringr)
library(ggplot2)
library(rCharts)
library(quantkit)
library(xts)

shinyServer(function(input,output){
  
  save <- reactive({FilterComponents(input$ticker, str_trim(str_split(input$components, ",")[[1]]), 
                             cutoff = input$pval, start = input$startDate, end = input$endDate)})
  
  output$timeseries <- renderChart({
    filter <- save()
    
    returns <- data.frame(date=as.character(index(filter$returns)), coredata(filter$returns))
    hPlot(x="date", y=input$ticker, data=returns, title=paste(input$ticker, "Filtered Returns"))
  })
  
  output$filteredPrice <- renderTable({
    filter <- save()
    if(!is.na(filter))
      filter$regression$coefficients
    else
      "No Significant Components"
  })
})