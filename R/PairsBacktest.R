#' Pairs Backtest
#'
#' @param ticker1 First equity ticker
#' @param ticker2 Second equity ticker
#' @param capital Starting cash
#' @param startTrain Beginning of correlation training period
#' @param endTrain End of correlation training period
#' @param startTest Beginning of backtest period
#' @param endTest End of backtest period
#' @return vector of account values
#' @keywords pairs, backtest, trade, statistical, arbitrage
#' @importFrom quantmod getSymbols
#' @export

PairsBacktest <- function(ticker1, ticker2, capital = 100000, threshold = 1.5,
                          startTrain = Sys.Date() - 365*2, endTrain = Sys.Date() - 365,
                          startTest = Sys.Date() - 365, endTest = Sys.Date()) {
  print("chare")
  account_values <- c()

  #Get price data
  stock_prices <- data.frame()
  for(stock in c(ticker1, ticker2))
  {
    prices <- data.frame(getSymbols(stock, from = startTrain, to = endTest, auto.assign = F, warnings = FALSE))
    prices$Date <- rownames(prices)
    stock_prices <-  if(nrow(stock_prices) == 0) prices[,c(4,7)] else merge(stock_prices, prices[,c(4,7)], by = "Date")
  }

  a_close  <- stock_prices[,2]
  b_close  <- stock_prices[,3]
  dates <- stock_prices$Date
  a_train <- a_close[dates <= endTrain & dates >= startTrain]
  b_train <- b_close[dates <= endTrain & dates >= startTrain]

  model <- lm(log(a_train) ~ log(b_train))
  mean_resid <- mean(res)
  sd_resid <- sd(res)
  intercept <- model$coefficients[1]
  slope <- model$coefficients[2]
  upper_bound <- threshold*sd_resid
  lower_bound <- -1* (threshold*sd_resid)

  a_price <- a_close[dates <= endTest & dates >= startTest]
  b_price <- b_close[dates <= endTest & dates >= startTest]


  residual <- log(a_price) - slope*log(b_price) - intercept

  a_num_shares <- 0
  b_num_shares <- 0

  testDates <- dates[dates <= endTest & dates >= startTest]

  a_short <- F
  hold <- F
  for (r in 1:length(residual)) {
    if(hold){
      if((residual[r] * residual[r-1]) <= 0){
        if(a_short)
          capital <- capital - a_num_shares * a_price[r] + b_num_shares * b_price[r]
        else
          capital <- capital - b_num_shares * b_price[r] + a_num_shares * a_price[r]

        a_num_shares <- 0
        b_num_shares <- 0

        hold <- F
      }
    }
    else if(residual[r] < lower_bound){
      a_num_shares <- floor((capital / 2) / a_price[r])
      b_num_shares <- floor((capital / 2) / b_price[r])

      capital <- b_price[r] * b_num_shares + capital
      capital <- -1 *(a_price[r] * a_num_shares) + capital
      a_short <- F
      hold <- T
    }
    else if(residual[r] > upper_bound){
      a_num_shares <- floor((capital / 2) / a_price[r])
      b_num_shares <- floor((capital / 2) / b_price[r])

      capital <- a_price[r] * a_num_shares + capital
      capital <- -1 *(b_price[r] * b_num_shares) + capital
      a_short <- T
      hold <- T
    }

    if(a_short)
      acct_val <- capital - a_num_shares * a_price[r] + b_num_shares * b_price[r]
    else
      acct_val <- capital - b_num_shares * b_price[r] + a_num_shares * a_price[r]

    account_values <- c(account_values, acct_val)
  }
  return(data.frame(Date=testDates, account_values=account_values, residuals=residual))
  
}
