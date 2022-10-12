# app_name <- 'TwitterSushantAnalysis'
# consumer_key <- 'iH1Nnry1Gs9MU4Ov9DLWnAndr'
# consumer_secret <- 'GCWmVDJnYo8kBGLLL5B4xKfMB3qu5JvcQoNPVziAK6r7QulwZK'
# access_token <- '1313485255966416899-lSkCe5VeEsj9PNdtLI4rZEpWfuAgnA'
# access_secret <- 'lXhMnpWKLFQNzvGGTFbHOKyClELHA0GqLuzDCqWuuuSoa'

# save(app_name, consumer_key,consumer_secret,access_token,access_secret,
#      file = 'C:/Users/sushk/Downloads/BU/MET CS 688/twitterAuthentication.Rdata')

#load("C:/Users/sushk/Downloads/BU/MET CS 688/twitterAuthentication.Rdata")

library(rtweet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(SnowballC)

token <- create_token(app = app_name,
             consumer_key = consumer_key,
             consumer_secret = consumer_secret,
             access_token = access_token, 
             access_secret = access_secret)

## **Top Gainers for the Day**
# 1. Search Tweets for Top Gainer Stock "$TSLA" on Oct 14th, 2020 -----------
twt.topgainer.1 <- search_tweets("$TSLA", n = 100, include_rts = FALSE, lang = "en")

# Filter the tweets to get secific columns to Inspect
twt.topgainer.1 <- twt.topgainer.1 %>% select(screen_name,text,created_at)

# Inspect the tibble
glimpse(twt.topgainer.1)



# 2. Search Tweets for Top Gainer Stock "$QCOM" on Oct 14th, 2020 -----------
twt.topgainer.2 <- search_tweets("$QCOM", n = 100, include_rts = FALSE, lang = "en")

# Filter the tweets to get secific columns to Inspect
twt.topgainer.2 <- twt.topgainer.2 %>% select(screen_name,text,created_at)

# Inspect the tibble
glimpse(twt.topgainer.2)



# 3. Search Tweets for Top Gainer Stock "$CSCO" on Oct 14th, 2020 -----------
twt.topgainer.3 <- search_tweets("$CSCO", n = 100, include_rts = FALSE, lang = "en")

# Filter the tweets to get secific columns to Inspect
twt.topgainer.3 <- twt.topgainer.3 %>% select(screen_name,text,created_at)

# Inspect the tibble
glimpse(twt.topgainer.3)



## **Top Losers for the Day**

# 1. Search Tweets for Top Loser Stock "$WFC" on Oct 14th, 2020 -----------
twt.toploser.1 <- search_tweets("$WFC", n = 100, include_rts = FALSE, lang = "en")

# Filter the tweets to get secific columns to Inspect
twt.toploser.1 <- twt.toploser.1 %>% select(screen_name,text,created_at)

# Inspect the tibble
glimpse(twt.toploser.1)




# 2. Search Tweets for Top Loser Stock "$BAC" on Oct 14th, 2020 -----------
twt.toploser.2 <- search_tweets("$BAC", n = 100, include_rts = FALSE, lang = "en")

# Filter the tweets to get secific columns to Inspect
twt.toploser.2 <- twt.toploser.2 %>% select(screen_name,text,created_at)

# Inspect the tibble
glimpse(twt.toploser.2)




# 3. Search Tweets for Top Loser Stock "$TMUS" on Oct 14th, 2020 -----------
twt.toploser.3 <- search_tweets("$TMUS", n = 100, include_rts = FALSE, lang = "en")

# Filter the tweets to get secific columns to Inspect
twt.toploser.3 <- twt.toploser.3 %>% select(screen_name,text,created_at)

# Inspect the tibble
glimpse(twt.toploser.3)


# save(twt.topgainer.1, twt.topgainer.2, twt.topgainer.3, twt.toploser.1, twt.toploser.2, twt.toploser.3, file = "C:/Users/sushk/Downloads/BU/MET CS 688/Term Project/TweetsFile")


load("C:/Users/sushk/Downloads/BU/MET CS 688/Term Project/TweetsFile")

# Top Gainers of the Day
# TSLA
glimpse(twt.topgainer.1)

# QCOM
glimpse(twt.topgainer.2)

# CSCO
glimpse(twt.topgainer.3)

# Top Losers of the Day
# WFC
glimpse(twt.toploser.1)

# BAC
glimpse(twt.toploser.2)

# TMUS
glimpse(twt.toploser.3)

twt.topgainer.1$Name <- "Tesla" 
twt.topgainer.2$Name <- "QUALCOMM" 
twt.topgainer.3$Name <- "Cisco" 

twt.toploser.1$Name <- "Wells Fargo" 
twt.toploser.2$Name <- "BOA" 
twt.toploser.3$Name <- "TMobile" 

twt.gainers <- rbind(twt.topgainer.1, twt.topgainer.2, twt.topgainer.3)

twt.losers <- rbind(twt.toploser.1, twt.toploser.2, twt.toploser.3)


# Remove any http elements from the tweets.
twt.gainers$stripped_text <- gsub("http\\S+", "", twt.gainers$text)

# Remove emojis AND other elements (other languages, etc.).
twt.gainers$stripped_text <- gsub("[^\u0020-\u007F]+", "", twt.gainers$stripped_text)

# I noticed &amp which was not getting removed by unnest_tokens, hence removing it manually.
twt.gainers$stripped_text <- gsub("&amp+", "", twt.gainers$stripped_text)

# Now we do some preprocessing on the tweets using unnest_tokens() function to convert to lowercase, remove punctuation, and add id for each tweet

# Also we will remove any stopwords from the tweets and store it in a cleaned data frame.

# Note that we are not stemming the tweets.

cleaned_twt.gainers <- twt.gainers %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%
  anti_join(stop_words)


twt.gainers$text[50]

twt.gainers$stripped_text[50]

# Now the same steps for Loser stock Tweets:
twt.losers$stripped_text <- gsub("http\\S+", "", twt.losers$text)
twt.losers$stripped_text <- gsub("[^\u0020-\u007F]+", "", twt.losers$stripped_text)
twt.losers$stripped_text <- gsub("&amp+", "", twt.losers$stripped_text)

cleaned_twt.losers <- twt.losers %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%
  anti_join(stop_words)

twt.losers$text[50]

twt.losers$stripped_text[50]



# Compute the most frequent terms for "Top Gainers" Tweets
freq.gainers <- cleaned_twt.gainers %>% 
  count(word, sort = TRUE) 
head(freq.gainers)

# Compute the most frequent terms for "Top Losers" Tweets
freq.losers <- cleaned_twt.losers %>% 
  count(word, sort = TRUE) 
head(freq.losers)


# Plot the most frequent words for "Gainers" and "Losers" tweets:

plot.gainers <- cleaned_twt.gainers %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "seagreen") +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Count",
       y = "Unique words",
       title = "Unique word counts found in Gainer stock tweets")

plot.losers <- cleaned_twt.losers %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "firebrick") +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Count",
       y = "Unique words",
       title = "Unique word counts found in Loser Stock tweets")

# We will plot them side by side for comparison
library(gridExtra)
grid.arrange(plot.gainers, plot.losers, ncol=2)


# Word Cloud for Gainers:
library(wordcloud)
wordcloud(freq.gainers$word[1:100], freq.gainers$n[1:100], colors=brewer.pal(8, "Dark2"), scale = c(4 , .1))
wordcloud(freq.losers$word[1:100], freq.losers$n[1:100], colors=brewer.pal(8, "Dark2"), scale = c(4 , .1))



# We will write the function sentiment_bing that takes in a tweet as parameter and we process the tweet for cleanup and then calculating the sentiment score.
sentiment_bing = function(twt){
  #Step 1;  perform basic text cleaning (on the tweet), as seen earlier
  twt_tbl = tibble(text = twt) %>% 
    mutate(
      # Remove http elements manually
      stripped_text = gsub("http\\S+","",text)
    ) %>% 
    unnest_tokens(word,stripped_text) %>% 
    anti_join(stop_words, by="word") %>%  #remove stop words
    inner_join(get_sentiments("bing"), by="word") %>% # merge with bing sentiment
    count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% 
    ## Create a column "score", that assigns a -1 one to all negative words, and 1 to positive words. 
    mutate(
      score = case_when(
        sentiment == 'negative'~ n*(-1),
        sentiment == 'positive'~ n*1)
    )
  ## Calculate total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, # if there are no words, score is 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negatives
  )
  ## This is to keep track of which tweets contained no words at all from the bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", # Type 1: no words at all, zero = no
    nrow(twt_tbl)>0~"Type 2" # Type 2: zero means sum of words = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}


# Compare sentiment for both "Gainers" and "Losers" tweet searches

# takes a minute or so
gainers_sent = lapply(twt.gainers$text,function(x){sentiment_bing(x)})
losers_sent = lapply(twt.losers$text,function(x){sentiment_bing(x)})


library(purrr)

stock_sentiment = bind_rows(
  tibble(
    StockType = 'Gainers',
    score = unlist(map(gainers_sent,'score')),
    type = unlist(map(gainers_sent,'type'))
  ),
  tibble(
    StockType = 'Losers',
    score = unlist(map(losers_sent,'score')),
    type = unlist(map(losers_sent,'type'))
  )
)

ggplot(stock_sentiment,aes(x=score, fill = StockType)) + geom_histogram(bins = 15, alpha = .6) +
  facet_grid(~StockType) + theme_bw()

stock_sentiment %>% group_by(StockType) %>% 
  summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )

## Now lets excluded the Type 1 tweets (tweets with no words in the bing list)

ggplot(stock_sentiment %>% filter(type != "Type 1"),aes(x=score, fill = StockType)) + geom_histogram(bins = 15, alpha = .6) +
  facet_grid(~StockType) + theme_bw()

stock_sentiment %>% filter(type != "Type 1") %>% group_by(StockType) %>% 
  summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )


# Do the results make sense?

# The summary and the plot helps us understand how the Type 2 tweets
# For Canada:
# We can see that the mean score is 0. This indicates that there are almost equal number of Positive and Negative tweets for "Canada".
# This can be confirmed if we look at the plot for "Canada" tweets as it looks almost Normally distributed, However it is more right skewed. Which means theres more negative tweets.

# For India:
# We can see that the mean score is 0.15. This indicates that the score of total Positive and Negative tweets for "India" are almost cancelled out.
# The plot for "India" tweets are left skewed as we can see more negative tweets compared to Positive tweets.

table(stock_sentiment %>% filter(type != "Type 1") %>% group_by(StockType))

# From the table below, we can see that "Canada" tweets have almost same number of Positive and Negative score tweets.
# "Canada" has about 6 tweets that were scored as Neutral.

# Also for "India" tweets, we can see that there are a bit more Positive tweets compared to their Negative score tweets.
# "India" tweets also have more positive tweets than "Canada", However there are more Type 2 tweets in "India" compared to "Canada" tweets
# There are 9 tweets that were scored as Neutral for "India"


