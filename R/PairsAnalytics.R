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