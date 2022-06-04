
requiredPackages = c('shiny','tm','ggplot2','wordcloud2','wordcloud','twitteR','openssl','ROAuth','httk','httpuv','plyr','stringr','leaflet','maps','deSolve')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p,repos = "http://cran.us.r-project.org", lib="/usr/local/lib/R/site-library")
  library(p,character.only = TRUE)
}



##library(shiny) 
##library(tm)
##library(wordcloud)
##library(twitteR)
##library(ROAuth)

##library(httk)
##library(httpuv)
##require(ggplot2)

##require(plyr)
##require(stringr)
##library(leaflet)
##library(maps)
##library(deSolve)

tweet_no<-100

##################################################################
# 1) clean tweet function

clean.tweet.modified <- function(tweet) {
  
  #tweet <- enc2utf8(tweet)
  #tweet <- iconv(tweet, 'utf-8', 'ascii', sub='')
  
  #tweet = gsub("[^\x20-\x7E]", "", tweet)
  
  tweet = gsub("https://","",tweet) # removes https://
  tweet = gsub("http://","",tweet) # removes http://
  tweet=gsub("[^[:graph:]]", " ",tweet) ## removes graphic characters 
  #like emoticons 
  tweet = gsub("[[:punct:]]", "", tweet) # removes punctuation 
  tweet = gsub("[[:cntrl:]]", "", tweet) # removes control characters
  tweet = gsub("\\d+", "", tweet) # removes numbers
  #tweet = gsub("\\t", "", tweet) # removes numbers
  #tweet = gsub("\\n", "", tweet) # removes numbers
  tweet = gsub("[ \t]{2,}", " ", tweet) # removes numbers
  tweet = gsub("^\\s+|\\s+$","", tweet)
  
  tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
  
  tweet = tolower(tweet) # makes all letters lowercase
  return(tweet)
  
}
###########################################################################
# 2) measure sentiment

score.sentiment.modified <- function(tweet, pos.words, neg.words){
  
  #tweet<-clean.tweet.modified (tweet) # clean Text
  
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

###################################################################
# 3) graph tweet by time
tweet.by.time<-function(trendingTweets.df){
  # plot on tweets by time
  plot<-ggplot(data = trendingTweets.df, aes(x = created)) +
    geom_histogram(aes(fill = ..count..)) +
    theme(legend.position = "none") +
    xlab("Time") + ylab("Number of tweets") + 
    scale_fill_gradient(low = "midnightblue", high = "aquamarine4")
  return(plot)
}

#######################################################
# 4) map of tweets
map.of.tweets<-function(trendingTweets.df){
  
  #trendingTweets.df<-trendingTweets.df[ !is.null(trendingTweets.df$longitude) && !is.null(trendingTweets.df$latitude),]
  
  # write.csv(trendingTweets.df,'healthstudy2.csv')
  
  
  trendingTweets.df<- filter(trendingTweets.df,!is.na(trendingTweets.df$longitude))
  
  #write.csv(trendingTweets.df,'trendingTweets.csv')
  
  m <- leaflet() %>% setView(lng = -2.637, lat = 52.862, zoom = 12)
  
  
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    setView(lng = -1.7983, lat = 51.5741, zoom = 5)
  
  
  
  ##  if( nrow(trendingTweets.df)!=0 ){
  ##      m <- leaflet(trendingTweets.df) %>% 
  ##        addTiles() %>%
  ##        addCircles(lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), popup = "The birthplace of R", weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)
  
  ##  } else{
  
  ##  m <- leaflet() %>%
  ##    addTiles() %>%  # Add default OpenStreetMap map tiles
  ##    setView(lng = -1.7983, lat = 51.5741, zoom = 5)
  ##   # addMarkers(lng=-1.7983, lat=51.5741, popup="The birthplace of R")
  ##  }
  ##    # Print the map 
  ##  summary(trendingTweets.df$latitude)
  return(m)
}
#######################################################


##########################################################

# Define UI for application that draws a histogram

#######################################################
ui<-fluidPage( 
  titlePanel("Sentiment Analysis"), #Title
  
  textOutput("currentTime"),   #Here, I show a real time clock
  
  #  sidebarPanel(
  wellPanel(
    textInput("search_text","Enter the any Text to search Twitter", width=NULL, placeholder=NULL),
    actionButton("submit","Submit")),
  #  ),
  
  
  h4("Tweets:"),   #Sidebar title
  sidebarLayout(
    
    sidebarPanel(
      DT::dataTableOutput('tweets_table') #Here I show the users and the sentiment
    ),
    
    mainPanel(
      
      plotOutput("distPlot"), #Here I will show the bars graph
      
      
      
      sidebarPanel(
        plotOutput("positive_wordcloud",height = "300px",width = "100%",
                   click = "plot_click",
                   hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                   brush = brushOpts(id = "plot_brush")
        ) #Cloud for positive words
      ),
      sidebarPanel(
        plotOutput("negative_wordcloud",height = "300px", width="100%") #Cloud for negative words
      ),
      sidebarPanel(
        plotOutput("neutral_wordcloud",height = "300px", width="100%") #Cloud for neutral words# 
      ),
      
      sidebarPanel(  
        width=300,
        plotOutput("tweets_by_time", height = "300px", width="auto") #Here I will show the bars graph
        
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(),br(), br(), br(),br(),br(),br(),
      #sidebarPanel(
      leafletOutput("tweets_by_location", width="1000px")   #Here I will show the map oftweets
      #)
      
    ) # mainPanel
    
    
    
    
    
    
  ) #sidebarLayout
)# fluid page
# )# UI

######################################################
server<- function(input, output, session) {
  
  api_key <- "FE2Zpf66P8LExaYvIbuW5YFgz"
  
  api_secret <- "czBueaQYcIqG9v0MiixmHQNNSHYQ5i6MzF9YzHsMDIU4QXayK2"
  
  access_token <- "2271052361-uclvKo9ekakleTts2plquz8QgNa95Rhq21xmtmt"
  
  access_token_secret <- "1RnnW9WzXsmizfAWfTjSQdQqSUyT7p8IRIiSGw76CFxRV"
  
  
  setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
  
  
  token <- get("oauth_token", twitteR:::oauth_cache)
  token$cache()
  
  ##########################################################################
  neg = scan("data/negative-words.txt", what="character", comment.char=";")
  pos = scan("data/positive-words.txt", what="character", comment.char=";")
  
  
  #########################################################################
  
  
  output$currentTime <- renderText({invalidateLater(2000, session)
    paste("Current time is: ",Sys.time())})
  input
  
  submit_results <- reactive({
    # Make sure requirements are met
    req(input$submit)
    paste(input$submit)
    temp1<-input$search_text
    return(temp1)
  })
  
  
  
  
  
  
  observe({
    invalidateLater(6000,session) #original setting 6000
    #reactiveTimer(2000)
    count_positive = 0
    count_negative = 0
    count_neutral = 0
    ###########################################    
    positive_text <- vector()
    negative_text <- vector()
    neutral_text <- vector()
    vector_users <- vector()
    vector_sentiments <- vector()
    vector_comments<-vector()
    ###########################################    
    tweets_results = ""
    tweets_results = searchTwitter(submit_results(),n=tweet_no)
    
    trendingTweets.df = twListToDF(tweets_results)
    
    trendingTweets.df$text<- sapply(trendingTweets.df$text, function(x) iconv(x,to='UTF-8'))
    #write.csv(trendingTweets.df,'healthstudy2.csv')
    
    # for loop for each tweet
    for (tweet in tweets_results){
      
      tweet$text<-clean.tweet.modified (tweet$text)
      
      print(paste(tweet$screenName, ":", tweet$text))
      
      
      vector_users <- c(vector_users, as.character(tweet$screenName));
      vector_comments<- c(vector_comments, as.character(tweet$text));
      
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
    
    df_text_sentiment<-  data.frame(vector_users,vector_comments, vector_sentiments)
    #################################################################
    # write file of results
    
    # write.table(df_text_sentiment, file="output.csv", append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = FALSE)
    
    ##################################################################
    output$tweets_table = DT::renderDataTable({
      df_users_sentiment
    })
    
    output$distPlot <- renderPlot({
      results = data.frame(tweets = c("Positive", "Negative", "Neutral"), numbers = c(count_positive,count_negative,count_neutral))
      barplot(results$numbers, names = results$tweets, xlab = "Sentiment", ylab = "Counts", col = c("Green","Red","Blue"))
      
      if (length(positive_text) > 0){
        #output$positive_wordcloud <- renderPlot({ wordcloud(paste(positive_text, collapse=" "),min.freq = 0 , random.color=TRUE, max.words=50 ,colors=palette(),rot.per=0.3,scale=c(4,.5))  })
        
        output$positive_wordcloud <- renderPlot({ wordcloud(paste(positive_text, collapse=" "), min.freq = 0, random.color=TRUE, max.words=100 ,colors=brewer.pal(8, "Set3"),rot.per=0.1,scale=c(8,.2))  })
        
        # output$positive_wordcloud <- renderPlot({ wordcloud2(positive_text)  })
        
      }
      if (length(negative_text) > 0) {
        
        
        output$negative_wordcloud <- renderPlot({ wordcloud(paste(negative_text, collapse=" "),random.color=TRUE,  min.freq = 0, max.words=100 ,colors=palette(),rot.per=0.1,scale=c(8,.2))  })
      }
      if (length(neutral_text) > 0){
        output$neutral_wordcloud <- renderPlot({ wordcloud(paste(neutral_text, collapse=" "), min.freq = 10, random.color=TRUE , max.words=100 ,colors=brewer.pal(8, "Dark2"))  })
      }
      
      
      output$tweets_by_time <-  renderPlot({tweet.by.time(trendingTweets.df) })
      
      
      output$tweets_by_location<- renderLeaflet({map.of.tweets(trendingTweets.df) })
      
      #output$tweets_by_location <- renderPlot({map.of.tweets(trendingTweets.df) })
      
      
    })
  })
  
  
}

######################################################




# Define server logic required to draw a histogram

# Run the application 
shinyApp(ui, server )
