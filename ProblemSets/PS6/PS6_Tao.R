library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(syuzhet)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(forcats)

requestURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "I2j9G106bfnJt88rSjrteTRGj"
consumerSecret = "1gxYmadllk95mJXyId0zoGoyHB8QJdHxy0eUey8s6EeFIIdrd7"
accessToken = "1055216750562820099-AnnRWiuJYV400GgPTh8zvcs1Vvlhat"
accessSecret = "h5Gk3w3nfrG7tk9GzMPaoHArXt52piBLTtuaH9dKAR32z"
setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessSecret)

tweets <- searchTwitter('iphone xs max', 
                        n=500, retryOnRateLimit=1)
tweets.df <- twListToDF(tweets) 
View(tweets.df)
head(tweets.df$text)

tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@.*","",tweets.df2)
tweets.df2 <- gsub("RT","",tweets.df2)
View(tweets.df2)

data <- data.frame(tweets = as.character(tweets.df2), 
                   stringsAsFactors = FALSE)
data <- data %>% 
  unnest_tokens(word, tweets)

sentiment <- get_sentiments("nrc")
data <- inner_join(data, sentiment, by = "word")
ggplot(data = data, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

pmf.tweets <- as.data.frame(prop.table(table(data$word)))
names(pmf.tweets) <- c("word", "freq")

wordcloud(pmf.tweets$word, pmf.tweets$freq, min.freq = 0.002) 

ggplot(pmf.tweets[pmf.tweets$freq>=0.002,], mapping = aes(x=reorder(word,freq),y=freq)) +
  geom_bar(stat="identity") +
  coord_flip()
