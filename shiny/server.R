library(shiny)
library(stringr)
library(rCharts)
library(quantkit)
library(xts)
library(quantmod)
library(corrplot)

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

    else if(input$charttype == "Unfiltered Price (Stock Price)"){
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
    CorrelationMatrix(input$similarticker, cutoff = input$regcutoff,
                      market.cap = input$mcap*.01, sector = input$sector, industry = input$industry)
  })

  spec_move <- reactive({
    if(input$event == "Large Moves")
      LargeMoves(input$specialticker, input$stddev, input$startDate2, input$endDate2)
    else if(input$event == "Earnings Moves")
      EarningsPerformance(input$specialticker, input$startDate2, input$endDate2)
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
    Ticker <- toupper(str_trim(str_split(input$tickerlist, ",")[[1]]))
    cbind(Ticker, CompareKeyStats(Ticker, input$quickstats))

  })

  output$weaksim <- renderDataTable({
    similarData <- similar()
    basket <- matrices()
    basket <- basket$highly_corr_stocks
    similarData[!similarData$Symbol %in% basket,]
  })

  output$strongsim <- renderDataTable({
    similarData <- similar()
    basket <- matrices()
    basket <- basket$highly_corr_stocks
    similarData[similarData$Symbol %in% basket,]
  })

  output$correlations <- renderPlot({
    mat <- matrices()
    corrplot(mat$cor_matrix)
  })

  output$covariances <- renderTable({
    mat <- matrices()
    mat$cov_matrix
  })

  output$summarymovement <- renderDataTable({
    summary <- spec_move()
    colnames(summary) <- c("Date", "Return")
    summary
  })

#   output$componentUI <- renderUI({
#     if(input$filtermovement)
#       textInput("specialcomponents", "Components (Comma separated)", "^GSPC")
#   })

  output$plots <- renderUI({
    if(input$event == "Large Moves")
      df <- spec_move()

    plot_list <- lapply(1:nrow(df), function(i){
      plotname <- paste("plot", i, sep="")
      showOutput(plotname, "Highcharts")
    })
    do.call(tagList, plot_list)
  })

  for (my_p in 1:50){
    local({
      p <- my_p
      plotname <- paste("plot", p, sep="")
      output[[plotname]] <- renderChart2({
        plotData <- getSymbols(input$specialticker, from = as.Date(spec_move()[p,1]) - input$window,
                   to = as.Date(spec_move()[p,1]) + input$window,
                   auto.assign = F)[,4]
        plotData <- data.frame(date = index(plotData), ticker = plotData[,1])
        plotData$date <- as.character(plotData$date)
        colnames(plotData) <- c("date", input$specialticker)
        hPlot(x="date", y=input$specialticker, data=plotData,
              title=paste(toupper(input$specialticker), "around", spec_move()[p,1]))
      })
    })
  }

})

