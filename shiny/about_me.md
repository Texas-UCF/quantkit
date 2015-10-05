# Quantkit | Texas UCF

Quantkit is a Shiny-powered R application to empower users to do their own statistical analysis with ease.  Note Quantkit is still a work-in-progress, and please voice and concerns to the Slack channel #quantkit.

## Functions
#### Filter Components
Filter components is useful for viewing an asset's price, and filtering out a certain component.  For example, we can enter "AAPL" and filter out ^GSPC (SP500) giving us the movement of Apple without the overall market changes.

In addition to filtering by the market or any component, the checkbox "Filter by Sector" automatically finds the given asset's sector and filters it by a common ETF of that sector, leaving the user with how the asset moved outside of its sector moves.

The output can be chosen between graphing just the stock price, filtered price, unfiltered and filtered daily returns, and unfiltered and filtered moments.

### Special Movements
Special movements is used to find large stock moves in order to dive deeper for analysis.  It inputs a ticker, a standard deviation that the move must have caused, a daterange, and lastly the window size for plotting.  

The event type Large Moves finds all moves greater than the given standard deviation in the date range.  Earnings Moves automatically finds earnings dates for the asset and displays and graphs them.

### Similar Stocks
Similar stocks is used for finding stocks similar to the entered ticker.  It utilizes how close the market caps should be, and matches for sector and industry as inputted.  Both strong and week similarities are outputted.

### Quick Facts
Quick Facts returns fundamental finance information for any equity (no ETFs, index funds, etc).




