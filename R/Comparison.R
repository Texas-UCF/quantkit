#' Comparing key stats for stocks
#'
#' @param symbols List of Yahoo Finance stock tickers
#' @param measures List of Key Stats to display. If empty display all that are available
#' @keywords similar, key stats, stocks
#' @export
#' @examples
#' CompareKeyStats(c("AAPL", "GOOG", "FB"), c("Operating Margin"))
CompareKeyStats <- function(symbols, measures) {
  if("All" %in% measures) {
    measures <- c(
      "Market Cap","Enterprise Value", "Trailing P/E",
      "Forward P/E","PEG Ratio","Price/Sales","Price/Book",
      "Enterprise Value/Revenue","Enterprise Value/EBITDA",
      "Fiscal Year Ends","Most Recent Quarter","Profit Margin","Operating Margin",
      "Return on Assets","Return on Equity","Revenue","Revenue Per Share",
      "Qtrly Revenue Growth","Gross Profit","EBITDA","Net Income Avl to Common",
      "Diluted EPS","Qtrly Earnings Growth","Total Cash","Total Cash Per Share",
      "Total Debt","Total Debt/Equity","Current Ratio","Book Value Per Share",
      "Operating Cash Flow","Levered Free Cash Flow",
      "Beta","52-Week Change","S&P500 52-Week Change",
      "52-Week High","52-Week Low","50-Day Moving Average",
      "200-Day Moving Average","Avg Vol","Avg Vol 1",
      "Shares Outstanding","Float","% Held by Insiders",
      "% Held by Institutions","Shares Short","Short Ratio",
      "Short % of Float","Shares Short 2","Forward Annual Dividend Rate",
      "Forward Annual Dividend Yield","Trailing Annual Dividend Yield",
      "Trailing Annual Dividend Yield 3","5 Year Average Dividend Yield",
      "Payout Ratio","Dividend Date","Ex-Dividend Date","Last Split Factor","Last Split Date"
    )
  }
  df <- data.frame(matrix(0, ncol = 58, nrow = 0))
  for(i in 1:length(symbols))
    df <- rbind(df, GetKeyStats(symbols[i]))
  return(df[measures])
}


#' get key stats for a ticker
#'
#' @param symbol Yahoo Finance stock ticker
#' @keywords key stats, stocks
#' @importFrom plyr ldply
#' @export
#' @examples
#' CompareKeyStats(c("AAPL", "GOOG", "FB"), c("Operating Margin"))
GetKeyStats <- function(symbol) {
  tickers <- c(symbol)
  stats <- ldply(tickers, GetKeyStatsHelper)
  rownames(stats) <- tickers
  return(stats)
}

#' used by GetKeyStats, credit to r-bloggers for this
#'
#' @param symbol Yahoo Finance stock ticker
#' @keywords key stats, stocks
#' @import XML
#' @export
#' @examples
#' CompareKeyStats(c("AAPL", "GOOG", "FB"), c("Operating Margin"))
GetKeyStatsHelper <- function(symbol) {
  yahoo.URL <- "http://finance.yahoo.com/q/ks?s="
  html_text <- htmlParse(paste(yahoo.URL, symbol, sep = ""), encoding="UTF-8")

  nodes <- getNodeSet(html_text, "/*//td[@class='yfnc_tablehead1']")

  if(length(nodes) > 0 ) {
   measures <- sapply(nodes, xmlValue)

   #Clean up the column name
   measures <- gsub(" *[0-9]*:", "", gsub(" \\(.*?\\)[0-9]*:","", measures))

   #Remove dups
   dups <- which(duplicated(measures))
   for(i in 1:length(dups))
     measures[dups[i]] = paste(measures[dups[i]], i, sep=" ")

   #use siblings function to get value
   values <- sapply(nodes, function(x)  xmlValue(getSibling(x)))

   df <- data.frame(t(values))
   colnames(df) <- measures
   return(df)
  }
}
