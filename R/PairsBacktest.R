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
  res <- as.vector(residuals(model))
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
  trainDates <- dates[dates <= endTrain & dates >= startTrain]

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

  data <- data.frame(Date=testDates, account_values=account_values, residuals=residual)
  data$Date = as.Date(data$Date)

  result <- list()
  result$ticker1 <- ticker1
  result$ticker2 <- ticker2

  result$data <- data
  result$model <- model
  result$lower_bound <- lower_bound
  result$upper_bound <- upper_bound

  trainData <- stock_prices[stock_prices$Date %in% trainDates,]
  testData <- stock_prices[stock_prices$Date %in% testDates,]

  colnames(trainData) <- c("Date", "a_train", "b_train")
  colnames(testData) <- c("Date", "a_test", "b_test")

  trainData$a_train <- log(trainData$a_train)
  trainData$b_train <- log(trainData$b_train)
  testData$a_test <- log(testData$a_test)
  testData$b_test <- log(testData$b_test)

  result$trainData <- trainData
  result$testData <- testData

  result$model <- model

  return(result)

}

ggplotRegression <- function (fit) {

  require(ggplot2)

  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

annualizeVolatility <- function(account_balances) {
  returns <- diff(account_balances)/head(account_balances,-1)
  return(sd(returns))*(sqrt(252))
}

annualizeReturns <- function(account_balances) {
  returns <- diff(account_balances)/head(account_balances,-1)
  returns <- returns + 1
  compoundedRet <- prod(returns)
  annualize = (compoundedRet^(252/length(returns)))
  return(annualize - 1)
}

sharpeRatio <- function(account_balances) {
  return(annualizeReturns(account_balances))/annualizeVolatility(account_balances)
}

# backtest <- PairsBacktest('MSFT', 'AAPL')
backtest_plot <- function(backtest){
  data <- backtest$data
  lower_bound <- backtest$lower_bound
  upper_bound <- backtest$upper_bound
  acct_vals <- data$account_values
  summary_df=data.frame(Benchmark=c("Annualized Returns", "Annualized Volatility", "Sharpe Ratio"),
                        Result=c(annualizeReturns(acct_vals), annualizeVolatility(acct_vals), sharpeRatio(acct_vals)))

  acct_plot <- ggplot(data, aes(x=Date, y=account_values)) + geom_line(size=1) + xlab("Date") +
    ylab("Account Values") + ggtitle("Account Value over Time")

  summary_table <- tableGrob(summary_df, rows=NULL)
  account_value_grid <- grid.arrange(acct_plot,summary_table, nrow=2, as.table=T)

  residuals_plot <- ggplot(data, aes(x=Date, y=residuals)) + geom_line(size=1) + xlab("Date") + ylab("Residual") +
    ggtitle("Residuals over Time") +
    geom_hline(yintercept=c(lower_bound, upper_bound), linetype='dashed', colour="#CC0000", size=2)

  regression_plot <- ggplot(data = backtest$trainData, aes(x=a_train, y=b_train)) +
    geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
    geom_point() + ggtitle("Regression") + xlab(paste("LN(",backtest$ticker1,")")) +
    ylab(paste("LN(", backtest$ticker2, ")"))

  plot_list <- list()
  plot_list$account_value_grid <- account_value_grid
  plot_list$residuals_plot <- residuals_plot
  plot_list$regression_plot <- regression_plot
  return(plot_list)
}

