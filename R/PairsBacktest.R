
backtest <- function(capital, a_open, a_close, b_open, b_close, mavg_window = 30) {
  account_values <- c()
  
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
  return(account_values)
  
}