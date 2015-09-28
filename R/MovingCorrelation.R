#' MovingCorrelation
#'
#' @param ticker Yahoo Finance target ticker
#' @param symbols vector of Yahoo Finance target tickets
#' @param start Start date
#' @param end End date
#' @return lists of correlation coefficients
#' @importFrom quantmod getSymbols dailyReturn
#' @export
#' @examples
#'MovingCorrelation("SDRL", c("RIG","DO"),'2014-09-01', '2015-05-25')

MovingCorrelation <- function(ticker, symbols, start = Sys.Date() - 365, end = Sys.Date()) {
  
  #
  timeframe <- 60
  
  all.data <- c()
  # Use quantmod to obtain price and returns 
  for(stock in c(ticker, symbols)){
    prices <- getSymbols(stock, from = start, to = end, auto.assign = FALSE)
    returns <- dailyReturn(prices, type = 'log')
    colnames(returns) <- stock
    
    if(length(all.data) == 0){
      all.data <- returns
    } else {
      all.data <- merge(all.data, returns)
    }
  }
  
  correl.data <- c()
  
  #Correlation Calculation
  for(i in 1:length(symbols)){
    df <- data.frame(matrix(0, ncol = 0, nrow = 0))
    for(j in timeframe:length(all.data[,1])){
      df <- rbind(df, cor(all.data[(j-timeframe+1):j,1], all.data[(j-timeframe+1):j,i+1]))
    }
    colnames(df) <- symbols[i]
    
    if(length(correl.data) == 0){
      correl.data <- df
    } else {
      correl.data <- cbind(correl.data, df)
    }
  }
  
  xrange <- range(length(correl.data[,1])/2)
  yrange <- range(correl.data)
  
  plot(xrange, yrange, type = "n", xlab = "Time", ylab = "Correlation Coefficient", main = "Correlation")
  colors <- rainbow(length(symbols))
  
  for(i in 1:length(symbols)){
    lines(correl.data[,i], type = "l", lwd=1.5, col=colors[i])
  }
  
  return(correl.data)
}