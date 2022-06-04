#https://analytics4all.org/2016/11/25/r-twitter-sentiment-analysis/


library(stringr)
library(twitteR)
library(xlsx)
library(plyr)
library(wordcloud)
library(ggplot2)
library(maps)

api_key <- "FE2Zpf66P8LExaYvIbuW5YFgz"

api_secret <- "czBueaQYcIqG9v0MiixmHQNNSHYQ5i6MzF9YzHsMDIU4QXayK2"

access_token <- "2271052361-uclvKo9ekakleTts2plquz8QgNa95Rhq21xmtmt"

access_token_secret <- "1RnnW9WzXsmizfAWfTjSQdQqSUyT7p8IRIiSGw76CFxRV"


setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


token <- get("oauth_token", twitteR:::oauth_cache)
token$cache()

setwd("/srv/shiny-server/twitter_demo")
neg = scan("data/negative-words.txt", what="character", comment.char=";")
pos = scan("data/positive-words.txt", what="character", comment.char=";")



tweets_results = ""
tweets_results = searchTwitter("Jordan",n=5)

Tweets.text = lapply(tweets_results,function(t)t$getText()) 

analysis = score.sentiment(Tweets.text, pos, neg)

score_tweet<-score.sentiment.modified(tweets_results[[4]]$text, pos, neg)

counts<-analysis$score
counts_table<-table(counts)

count_positive <-0
count_negative <-0
count_neutral  <-0

positive_text <- vector()
negative_text <- vector()
neutral_text <- vector()
vector_users <- vector()
vector_sentiments <- vector()

###################################################################
for (tweet in tweets_results){
  print(paste(tweet$text))
}

analysis
for (tweet in tweets_results){
  print(paste(tweet$screenName, ":", tweet$text))
   vector_users <- c(vector_users, as.character(tweet$screenName));
   #if (grepl("lindo", tweet$text, ignore.case = TRUE) == TRUE | grepl("Wonderful", tweet$text, ignore.case = TRUE) | grepl("Awesome", tweet$text, ignore.case = TRUE)){
   if((positive=score.sentiment.modified(tweet$text, pos, neg))>0){
      count_positive = count_positive + 1
   #print("positivo")
    vector_sentiments <- c(vector_sentiments, "Positive")
    positive_text <- c(positive_text, as.character(tweet$text))
  } else if((negative=score.sentiment.modified(tweet$text, pos, neg))<0) { 
    count_negative = count_negative + 1
    print("negativo")
    vector_sentiments <- c(vector_sentiments, "Negative")
    negative_text <- c(negative_text, as.character(tweet$text))
  } else if ((neutral=score.sentiment.modified(tweet$text, pos, neg))==0){
    count_neutral = count_neutral + 1
    print("neutral")
    vector_sentiments <- c(vector_sentiments, "Neutral")
    neutral_text <- c(neutral_text, as.character(neutral_text))
  }
}
df_users_sentiment <- data.frame(vector_users, vector_sentiments)


results = data.frame(tweets = c("Positive", "Negative", "Neutral"), numbers = c(count_positive,count_negative,count_neutral))
barplot(results$numbers, names = results$tweets, xlab = "Sentiment", ylab = "Counts", col = c("Green","Red","Blue"))

plot.locations(tweets_results)

####################################################################################


score.sentiment.modified <- function(tweet, pos.words, neg.words){
  
  require(plyr)
  require(stringr)
    tweet
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
  }


score.sentiment <- function(tweets, pos.words, neg.words){
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    
    
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}


plot.locations<-function(tweetFrame){

  
  userInfo <- lookupUsers(tweetFrame$screenName)  # Batch lookup of user info
  
  userFrame <- twListToDF(userInfo)  # Convert to a nice dF
  
  
  
  locatedUsers <- !is.na(userFrame$location)  # Keep only users with location info
  
  
  
  locations <- geocode(userFrame$location[locatedUsers])  # Use amazing API to guess
  
  # approximate lat/lon from textual location data.
  
  with(locations, plot(lon, lat))
  
  
  
  worldMap <- map_data("world")  # Easiest way to grab a world map shapefile
  

  
  zp1 <- ggplot(worldMap)
  
  zp1 <- zp1 + geom_path(aes(x = long, y = lat, group = group),  # Draw map
                         
                         colour = gray(2/3), lwd = 1/3)
  
  zp1 <- zp1 + geom_point(data = locations,  # Add points indicating users
                          
                          aes(x = lon, y = lat),
                          
                          colour = "RED", alpha = 1/2, size = 1)
  
  zp1 <- zp1 + coord_equal()  # Better projections are left for a future post
  
  zp1 <- zp1 + theme_minimal()  # Drop background annotations
  
  print(zp1)
}


