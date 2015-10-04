library(shiny)
library(stringr)
library(rCharts)
library(quantkit)
library(xts)
library(quantmod)

shinyServer(function(input,output){
  
  filterData <- reactive({
    FilterComponents(input$ticker, str_trim(str_split(input$components, ",")[[1]]), 
                               cutoff = input$pval, start = input$startDate, end = input$endDate)
  })
  
  hedgeData <- reactive({
    HedgeTicker(input$ticker, numshares = input$hedgeshares, 
                components = str_trim(str_split(input$components, ",")[[1]]), 
                cutoff = input$pval, start = input$startDate, end = input$endDate)
  })
  
  dataInput <- reactive({
    filter <- filterData()
    price <- getSymbols(input$ticker, from=input$startDate, to=input$endDate, auto.assign = F)[,4]    
    
    if(input$charttype == "Filtered Returns")
        data.frame(date=as.character(index(filter$returns)), coredata(filter$returns))
    
    else if(input$charttype == "Filtered Price")
        data.frame(date=as.character(index(filter$filtered.price)), coredata(filter$filtered.price))
    
    else if(input$charttype == "Unfiltered Price"){
      result <- data.frame(date=as.character(index(price)), coredata(price))
      colnames(result) <- c("date", input$ticker)
      result
    }
    
    else if(input$charttype == "Unfiltered Returns"){
      result <- data.frame(date=as.character(index(price)), coredata(dailyReturn(price)))
      colnames(result) <- c("date", input$ticker)
      result
    }
#     else{
#       output$regressionTable <- renderTable({
#         if(!is.na(filter))
#           filter$regression$coefficients
#         else
#           "No Significant Components"
#       })  
#     }
  })
  
  
  
  output$regressionstats <- renderTable({
    fd <- filterData()
    table <- cbind(fd$regression$coefficients, c(NA, hedgeData()))
    colnames(table) <- c(colnames(fd$regression$coefficients), "Hedge Shares")
    table
  })
      
  output$plot <- renderChart2({
    print(dataInput())
    hPlot(x="date", y=input$ticker, data=dataInput(), title=paste(input$ticker, input$charttype))
  })
})