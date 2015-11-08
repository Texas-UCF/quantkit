#' Pairs Backtest
#'
#' @param capital Starting cash
#' @param a_open Open price series for ticker A
#' @param b_open Open price series for ticker B
#' @param a_close Close price series for ticker A
#' @param b_close Close price series for ticker B
#' @return vector of account values 
#' @keywords pairs, backtest, trade, statistical, arbitrage
#' @importFrom quantmod getSymbols
#' @export

PairsBacktest <- function(capital, ticker1, ticker2, mavg_window = 30, start = Sys.Date() - 365, end = Sys.Date()) {
  account_values <- c()
  
  #Get price data
  stock_prices <- data.frame() 
  for(stock in c(ticker1, ticker2))
  {
    prices <- data.frame(getSymbols(stock, from = start, to = end, auto.assign = F, warnings = FALSE))
    prices$Date <- rownames(prices)
    stock_prices <-  if(nrow(stock_prices) == 0) prices[,c(1,4,7)] else merge(stock_prices, prices[,c(1,4,7)], by = "Date")
  }
  
  a_open  <- stock_prices[,1]
  a_close  <- stock_prices[,2]
  b_open  <- stock_prices[,3]
  b_close  <- stock_prices[,4]
  
  for (i in (mavg_window+1):length(a_open)) {
    #Calculate price differences for previous 'days' days
    diffs = b_close((i-mavg_window):(i-1)) - a_close((i-mavg_window):(i-1))
    
    #Take average to find average moving day price difference
    mean_diff = mean(diffs)
    
    #Calculate current price difference from yesterday's close prices
    curr_diff = b_close(i-1) - a_close(i-1)
    
    #Get share ratios 
    b_shares = (capital / 2) / b_open
    a_shares = (capital / 2) / a_open
    
    if (curr_diff > mean_diff) {
      #Short B, Buy A - expect prices to converge and close position at market close
      capital = capital + b_shares * (b_open(i) - b_close(i)) + a_shares * (a_close(i) - a_open(i))
    } 
    
    else {
      #Buy B, Short A - expect prices to diverge and close position at market close
      capital = capital + b_shares * (b_close(i) - b_open(i)) + a_shares * (a_open(i) - a_close(i))
    }
    
    account_values <- c(account_values, capital)
  }
  return data.frame(Date=stock_prices$Date, account_values=account_values)
  
}