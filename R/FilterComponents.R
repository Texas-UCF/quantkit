#' Filter Components.
#'
#' @param ticker Yahoo Finance target ticker
#' @param components Vector of Yahoo Finance tickers to be filtered out
#' @param cutoff If p-value > cutoff, exclude the component
#' @param start Start date
#' @param end End date
#' @return List of filtered out time series, returns, and regression results
#' @keywords regression, components
#' @importFrom quantmod getSymbols dailyReturn
#' @export
#' @examples
#' FilterComponents("XOM", c("^GSPC", "USO"), 0.05, as.Date('2015-01-01'), as.Date('2015-05-25'))

FilterComponents <- function(ticker, components = "^GSPC", cutoff = 0.05, start = Sys.Date() - 365, end = Sys.Date(), sec = FALSE) {
  
  all.data <- c()
  
  if(sec == TRUE){
    df <- stockSymbols(quiet = TRUE)
  
    # Fix market cap column
    df$Market.Cap <- as.numeric(gsub("[\\$MB]", "", df$MarketCap))
    df[grep("B", df$MarketCap), 'Market.Cap'] <- df[grep("B", df$MarketCap), 'Market.Cap'] * 1000
    df <- df[, c('Symbol', 'Name', 'Market.Cap', 'Sector', 'Industry')]
    
    # Get targets
    row <- df[which(df$Symbol == ticker), ]
    
    sector <- row["Sector"]

    if(sector == "Transportation"){
      components <- c(components, c("IYT"))
    }else if(sector == "Technology"){
      components <- c(components, c("IYW"))
    }else if(sector == "Public Utilities"){
      components <- c(components, c("IDU"))
    }else if(sector == "Miscellaneous"){
      components <- c(components, c("IGF"))
    }else if(sector == "Health Care"){
      components <- c(components, c("IYH"))
    }else if(sector == "Finance"){
      components <- c(components, c("IYF"))
    }else if(sector == "Energy"){
      components <- c(components, c("IYE"))
    }else if(sector == "Consumer Services"){
      components <- c(components, c("IYC"))
    }else if(sector == "Consumer Non-Durables"){
      components <- c(components, c("KXI"))
    }else if(sector == "Consumer Durables"){
      components <- c(components, c("RXI"))
    }else if(sector == "Capital Goods"){
      components <- c(components, c("EXI"))
    }else if(sector == "Basic Industries"){
      components <- c(components, c("IYM"))
    }else{
      
    }
  }
  
  
  # Use quantmod to get all the stock prices
  for (stock in c(ticker, components)) {
    prices <- getSymbols(stock, from = start, to = end, auto.assign = F, warnings = F)
    rets <- dailyReturn(prices)
    colnames(rets) <- stock
    
    if (length(all.data) == 0) {
      all.data <- rets
    } else {
      all.data <- merge(all.data, rets)
    }
  }
  
  all.data <- all.data[complete.cases(all.data), ]
  
  # Run Regression
  regression <- summary(lm(all.data[, 1] ~ ., data = all.data[, -1]))
  
  # Remove variables where p value > cutoff
  init.coeff <- regression$coefficients
  sig.comp <- c(colnames(all.data)[1], 
                intersect(rownames(init.coeff)[init.coeff[,'Pr(>|t|)'] < cutoff], colnames(all.data)))
  
  # Re-Run Regression if variables were removed
  if(length(sig.comp) > 1) {
    sig.returns <- all.data[, sig.comp]
    sig.regression <- summary(lm(sig.returns[, 1] ~ ., data = sig.returns[, -1]))
    
    # Use regression results to filter out time series
    fin.coeff <- sig.regression$coefficients[, "Estimate"]
    y.hat <- rowSums(t(t(sig.returns[, -1]) * fin.coeff[2:length(fin.coeff)]))
    resid <- sig.returns[, 1] - y.hat
    result <- list()
    result$returns <- resid
    
    price <- 1
    resid[1] <- price
    for(i in 2:nrow(resid)){
      resid[i] <- price * as.numeric(resid[i]) + price
      price <- resid[i]
    }
    result$filtered.price <- resid
    result$regression <- sig.regression
    
    return(result)   
  } else {
    return(NA)
  }
}