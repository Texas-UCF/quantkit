library(shiny)
library(stringr)
library(rCharts)
library(quantkit)
library(xts)
library(quantmod)
library(qmao)
library(corrplot)
library(markdown)

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
    else if(input$charttype == "Unfiltered Return Moments"){

    }
    else if(input$charttype == "Filtered Return Moments"){

    }

  })
  
  pairsDataInput <- reactive({
    result <- PairsBacktest(input$capital, input$pairsTicker1, input$pairsTicker2, 
                            input$pairsThreshold, input$startDateTrain, 
                            input$endDateTrain, input$startDateTest, input$endDateTest)
    result
  })

  similar <- reactive({
    SimilarStocks(input$similarticker, input$mcap*.01, input$sector, input$industry)
  })

  matrices <- reactive({
    CorrelationMatrix(input$similarticker, cutoff = input$regcutoff,
                      market.cap = input$mcap*.01, sector = input$sector, industry = input$industry)
  })

  moments <- reactive({
    ReturnMoments(input$ticker, start = input$startDate, end=input$endDate)
  })

  spec_move <- reactive({
    if(input$event == "Large Moves")
      LargeMoves(input$specialticker, input$stddev, input$startDate2, input$endDate2)
    else if(input$event == "Earnings Moves")
      EarningsPerformance(input$specialticker, input$startDate2, input$endDate2)
  })

  output$regressionstats <- renderTable({
    if(!((input$charttype == "Unfiltered Return Moments")||(input$charttype == "Filtered Return Moments"))){
      fd <- filterData()
      table <- cbind(fd$regression$coefficients, c(NA, hedgeData()))
      colnames(table) <- c(colnames(fd$regression$coefficients), "Hedge Shares")
      table
    }

  })

  output$histPlot <- renderPlot({
    if(input$charttype == "Unfiltered Return Moments"){
      rets <- dailyReturn(getSymbols(input$ticker, from = input$startDate, to = input$endDate, auto.assign = FALSE),type = 'log')
      hist(as.vector(rets), breaks = "FD", col = "red", xlab = "Logarithmic daily returns",
           ylab = "Occurrences", main = paste(input$ticker, " return distribution with normal curve"))
      xfit <- seq(min(rets), max(rets), length = 40)
      yfit <- dnorm(xfit, mean = mean(rets), sd = sd(rets))
      lines(xfit, yfit, col="blue", lwd = 2)
    }
    else if(input$charttype == "Filtered Return Moments"){
      rets <- filterData()
      table1 <- cbind(rets$returns)
      hist(as.vector(table1), breaks = "FD", col = "red", xlab = "Logarithmic daily returns",
           ylab = "Occurrences", main = paste(input$ticker, " return distribution with normal curve"))
      xfit <- seq(min(table1), max(table1), length = 40)
      yfit <- dnorm(xfit, mean = mean(table1), sd = sd(table1))
      lines(xfit, yfit, col="blue", lwd = 2)
    }

  })

  output$retMoments <- renderTable({
    if(input$charttype == "Unfiltered Return Moments"){
      mdata <- moments()
      moutput <- matrix(unlist(mdata),ncol = 4, byrow = T)
      colnames(moutput) <- c("Mean", "SD", "Skewness", "Kurtosis")
      moutput
    }
    else if(input$charttype == "Filtered Return Moments"){
      rets <- filterData()
      table2 <- cbind(rets$returns)
      mdata <- list("mean" = mean(table2), "sd" = sd(table2))
      moutput <- matrix(unlist(mdata),ncol = 2, byrow = T)
      colnames(moutput) <- c("Mean", "SD")
      moutput

    }
  })

  output$plot <- renderChart2({
    plot<-hPlot(x="date", y=input$ticker, data=dataInput(), title=paste(input$ticker, input$charttype))
    plot$xAxis(title=list(text=paste(input$startDate, "  to   ", input$endDate)),type="datetime", labels = list(enabled = F))
    return(plot)
  })

#   output$pairsplot <- renderChart2({
#     plot<-hPlot(x="Date", y="account_values", data=pairsDataInput(), 
#                 title=paste(input$ticker, input$charttype))
#     # plot$xAxis(title=list(text=paste(input$startDate, "  to   ", input$endDate)),type="datetime", labels = list(enabled = F))
#     return(plot)
#   })
  
  output$pairs_info <- renderTable({
    return(PairsBacktest(input$capital, input$pairsTicker1, input$pairsTicker2, 
                            input$pairsThreshold, input$startDateTrain, 
                            input$endDateTrain, input$startDateTest, input$endDateTest))
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
    if(input$event == "Earnings Moves")
      summary <- spec_move()$data[6,]
    else
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

    if(input$event == "Earnings Moves"){
      df <- data.frame(spec_move()$data[6,])
    }

    plot_list <- lapply(1:nrow(df), function(i){
      plotname <- paste("plot", i, sep="")
      showOutput(plotname, tolower("Highcharts"))
    })
    do.call(tagList, plot_list)
  })

  for (my_p in 1:50){
    local({
      p <- my_p
      plotname <- paste("plot", p, sep="")
      output[[plotname]] <- renderChart2({

        if(input$event == "Earnings Moves")
          sm <- spec_move()$data[6,]
        else
          sm <- spec_move()

        plotData <- getSymbols(input$specialticker, from = as.Date(sm[p,1]) - input$window,
                   to = as.Date(sm[p,1]) + input$window,
                   auto.assign = F)[,4]
        plotData <- data.frame(date = index(plotData), ticker = plotData[,1])
        plotData$date <- as.character(plotData$date)
        colnames(plotData) <- c("date", input$specialticker)
        return(hPlot(x="date", y=input$specialticker, data=plotData,
              title=paste(toupper(input$specialticker), "around", sm[p,1])))
      })
    })
  }

})

