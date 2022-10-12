# Twitter-stock-market-sentiment-analysis using R

Project on Twitter Stock Market Sentiment Analysis for 3 Stocks that were Top Gainers and 3 Stocks that were Top Losers on Oct 14th, 2020. 

I have referred https://finance.yahoo.com/ to identify the top Day Gainers and Losers. The idea behind the project is to identify these Top Gainer and Loser stocks on a particular day and extract the tweets using **Twitter API via R**. We will be doing the Twitter Social Media Sentiment Analysis of these Stocks and to extract them from Twitter, we will be using the cashtags. The cashtags easily identify the Stock symbols for these companies and the tweets / data that we will search for will be more relevant to stock market sentiments on these Gainers and Losers of the Day.

We have considered the Most Active Stock symbols in United States on the given day Oct 14th, 2020.

The filters we have applied to get the below Most Active in United States Market - Top Gainers and Top Losers of the Day are as follows:

Region = United States

Market Cap (Intraday) = Mega Cap

Volume (Intraday) greater than 5000000
