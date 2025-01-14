# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.

# Update your quantmod library, or you might not be able to download the data.
# version should be 0.4.2.
# Here is a link to discussion of the problem with getting data from yahoo.
# https://github.com/joshuaulrich/quantmod/issues/358

#####Problem 1#####
# Write the following functions:
# 1.1. A function which replicates the SMA function. It should calculate the SMA
# for a vector to the left:
# 1.2. A function, which calculates the correlation coefficient between two vectors.
# It should replicate the cor function from the base package.

library(tidyverse) 
library(tidyquant) 
library(lubridate) 
library(quantmod) 

# 1.1.
library(TTR)

price = c(3, 9, 1, 10, 15, 4)
newSMA = function (price,n){
  f = c()
  for (i in n:length(price)){
    f[i] = mean(price[(i-n+1):i])
  }
  return(f)
} 

newSMA(price, n=2) 

SMA(price, n=2)

# 1.2. 
a = InputVector
b = c(3, 4, 5, 1, 2, 11, 13)
b = InputVector1
correlation = function(InputVector, InputVector1){
  Sumdiff = length(InputVector)*sum(InputVector*InputVector1)-sum(InputVector)*sum(InputVector1)
  FinalResult = Sumdiff/sqrt((length(InputVector)*sum(InputVector^2) - sum(InputVector)^2)*(length(InputVector1)*sum(InputVector1^2)-sum(InputVector1)^2))
  return(FinalResult)
}

correlation(a,b) 

cor(a, b)

#####Problem 1#####


#####Problem 2#####
# Find all prime numbers less than 100, using for/while loops.

# I don't understand how to do this one 

#####Problem 2#####


#####Problem 3#####
# Read the wikipedia article and investopedia article on MACD:
# https://en.wikipedia.org/wiki/MACD
# https://www.investopedia.com/terms/m/macd.asp

# Download data for a stock of your choice and do the following:
# 1.Calculate the 26-period EMA(use the EMA function from tidyquant)
# 2.Calculate the 12-period EMA.
# 3.Calculate the MACD line(12-period EMA minus 26-period EMA)
# 4.Calculate the signal line - this is the 9-period EMA of the MACD.
# 5.Calculate the buy/sell signals. This means create a new column which tell
# us if we should buy or sell. When the MACD line crosses the signal line
# from above(MACD is above signal then MACD is below signal) this is a sell signal. 
# If it crosses from below (MACD is below signal then MACD is above signal) this is a buy signal.
# 6. Simulate how the strategy preforms and compare it to a benchmark strategy
# of just buying and holding the stock.
# In order to do this start with a portfolio of 100$ invested in the stock on the first day
# and see how it performs. Example:
# I start with 100$ and a stock which costs 100$ at the beginning of my time period.
# I get a buy signal when the stock price is 90. I buy the stock.
# I get a sell signal to sell the stock when the price is 110. I sell it and 
# and don't get any more signals.I end up with 100 * 110 / 90 = 122.22 
# The benchmark portfolio is I buy the stock at 100 at the beginning and at
# the end of the period the stock price is 120. I end up with 120.
# 122.22 > 120. so the MACD strategy was beating the market.

#I cannot do this task, as well. I searched in google how to do it and I can kinda see the idea,
#however, I don't think I can replicate it entirely.

#####Problem 3#####












