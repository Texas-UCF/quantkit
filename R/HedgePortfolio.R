#' Hedge Portfolio
#'
#' @param portfolio character vector of Yahoo Finance tickers
#' @param hedgePositions 1 : long on ticker, 0 : no exposure to ticker, -1 : short on ticker
#' @param cutoff If p-value > cutoff, exclude the portfolio component
#' @param start Start date
#' @param end End date
#' @return numeric vector of hedge ratios for entire portfolio
#' @keywords regression, components
#' @importFrom quantmod getSymbols
#' @export
#' @examples
#' HedgePortfolio(c('SDRL', 'SDLP', 'SPY', 'USO'), c(-1, 1, 0, 0))

HedgePortfolio <- function(portfolio, hedgePositions, cutoff=0.05, start=Sys.Date()-365, end=Sys.Date()){
  input <- matrix()
  for(ticker in 1:length(portfolio)){
    #Each column in the input matrix is the time series for each ticker in the portfolio
    input_price <- getSymbols(portfolio[ticker], from = start, to = end, auto.assign = F, warnings = F)
    input <- cbind(input, input_price[,4])
    
    #Get desiredoutput by combining the weighted filtered components time series for each ticker
    #TODO: Check if this linear algebra works out 
    filter <- FilterComponents(portfolio[ticker], components=portfolio[portfolio != portfolio[ticker]], 
                               cutoff = cutoff, start=start, end=end)
    output_price <- filter$filtered.price * hedgePositions[ticker]
    if(ticker == 1) 
      output = output_price
    else 
      output <- output + output_price
  }
  input <- input[,2:ncol(input)]
  
  #Find Least Squares Fit between the input and output to get hedging ratios
  #TODO: results don't have any clear interpretation
  hedges <- lsfit(input, output)
  hedges <- hedges$coefficients
  names(hedges) <- c("Intercept", portfolio)
  return(hedges)
}

