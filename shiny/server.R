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
  
  })
  
  similar <- reactive({
    SimilarStocks(input$similarticker, input$mcap*.01, input$sector, input$industry)
  })
  
  matrices <- reactive({
    CorrelationMatrix(input$similarticker)
  })
  
  output$regressionstats <- renderTable({
    fd <- filterData()
    table <- cbind(fd$regression$coefficients, c(NA, hedgeData()))
    colnames(table) <- c(colnames(fd$regression$coefficients), "Hedge Shares")
    table
  })
      
  output$plot <- renderChart2({
    hPlot(x="date", y=input$ticker, data=dataInput(), title=paste(input$ticker, input$charttype))
  })
  

  
  output$factsTable <- renderDataTable({
    tickers <- str_trim(str_split(input$tickerlist, ",")[[1]])
    names(tickers) <- "Ticker"
    cbind(tickers, CompareKeyStats(tickers, input$quickstats))

  })
  
  output$weaksim <- renderDataTable({
    similarData <- similar()
    basket <- matrices()
    basket <- basket$highly_corr_stocks
    # if(length(basket) > 0) 
      similarData[!similarData$Symbol %in% basket,]
    # else
      # data.frame()
  })
  
  output$strongsim <- renderDataTable({
    similarData <- similar()
    basket <- matrices()
    basket <- basket$highly_corr_stocks
    # if(length(basket) > 0)
      similarData[similarData$Symbol %in% basket,]
    # else
      # data.frame()
  })
  
  output$correlations <- renderTable({
    mat <- matrices()
    mat$cor_matrix
  })
  
  output$covariances <- renderTable({
    mat <- matrices()
    mat$cov_matrix
  })
})
