library(twitteR)
library(httr)
library(lubridate)
library(ggmap)

setup_twitter_oauth(consumer_key    = jkf::key_chain('twitter.api'),
                    consumer_secret = jkf::key_chain('twitter.api.secret'),
                    access_token    = jkf::key_chain('twitter.token'),
                    access_secret   = jkf::key_chain('twitter.token.secret'))

tweets <- searchTwitter('#gopats', 
                        geocode = '42.375,-71.1061111,10mi')

lat_lon_dis <- function(location = 'Dayton, OH', distance = '20mi') {
  
  tw_ll <- ggmap::geocode(location)
  paste(c(tw_ll[2], tw_ll[1], distance), collapse = ',')
  
} 

tw_lat_long
# Convert List of tweets to data.frame
tweet_df <- twListToDF(tweets)
View(tweet_df)



# look up tweet
check_tweet <- function(tweet) {

  id <- tweet$id
browseURL(paste0('https://twitter.com/statuses/',id)
          
}
getTimeOfTweets <- function(hashtag, 
                            numberToScrape, 
                            dateTo, 
                            dateFrom, 
                            max = NULL) {
  
  
  tweets <- searchTwitter(hashtag, n=numberToScrape, lang="en", since = dateTo, until = dateFrom, maxID = max)
  tweetsDF <- twListToDF(tweets)
  tweetCreated <- ymd_hms(tweetsDF$created)
  ESTTweetCreated <- with_tz(tweetCreated, tzone = "America/New_York")
  tweetDFOut <- data.frame(tweetCreated, 
                           ESTTweetCreated, 
                           tweetsDF$text, 
                           tweetsDF$id,
                           stringsAsFactors=FALSE)
  
}