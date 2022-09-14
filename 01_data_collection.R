# Library 
library(rtweet) # collect tweets 
library(data.table) # setnames
library(stringr)
library(tidyverse)
library(sf)
library(sp)
library(leaflet)
library(tigris)
library(tidytext)

library(twitteR) 
library(ROAuth)
library(RCurl)

library(httpuv)
library(base64enc)
library(igraph)    # netwlrk 
library(tidygraph)
library(ggraph)

library(plyr)
library(stringr)


library(tmap)
library(tmaptools)
library(OpenStreetMap)
library(shinyjs)
library(jsonlite)

library(httr)
library(dplyr)
library("rjson")
library("magrittr")
library("data.table")

# Melt the correlation matrix
library(reshape2)

# ggplot 
library(ggplot2)
library(hrbrthemes)
library(gridExtra)
library(ggbreak) 
library(hrbrthemes)
library(ggpubr)

# network 
library(igraph)
library(jsonlite)
library(data.table) # setnames
library(tidyr)
library(forcats)
library(viridis)
library(tibble)

# Spatial Test
library(spdep) 
library(SpatEntropy)
library("RColorBrewer")
library(rosm)

# FUNCTION - request tweets using full-archive-search 
request_tweet_vax <- function(start_date, end_date){
  params = list(
    `query` = 'place:",NY" ("vaccine" OR "vax" OR "moderna" OR "pfizer" OR "johnson&johnson" OR "j&j" OR "AstraZeneca") lang:en',
    `start_time` = paste(start_date,'T00:00:00.00Z', sep = ""),
    `end_time` = paste(end_date,'T23:59:59.00Z', sep = ""),
    `max_results` = '500',
    `tweet.fields` = 'author_id,public_metrics,created_at,lang,conversation_id,geo,in_reply_to_user_id,referenced_tweets',
    `expansions` = 'author_id,geo.place_id',
    `place.fields` = 'geo',
    `user.fields` = 'created_at'
  )
  
  response <- httr::GET(url = edp_all, httr::add_headers(.headers=headers), query = params)
  
  recent_vax <-
    content(
      response,
      as = 'parsed',
      type = 'application/json',
      simplifyDataFrame = TRUE
    )
  
  return(recent_vax)
}


# FUNCTION - search tweets by conversation_id
request_conversation <- function(conversation_id, start_date){
  params = list(
    `query` = paste('conversation_id:',conversation_id, sep = ""),
    `start_time` = paste(start_date,'T00:00:00.00Z', sep = ""),
    `max_results` = '500',
    `tweet.fields` = 'in_reply_to_user_id,author_id,public_metrics,created_at,lang,geo,conversation_id',
    `expansions` = 'geo.place_id',
    `place.fields` = 'geo'
  )
  
  response <- httr::GET(url = edp_all, httr::add_headers(.headers=headers), query = params)
  recent_vax <-
      content(
        response,
        as = 'parsed',
        type = 'application/json',
        simplifyDataFrame = TRUE
      )
  
  return(recent_vax)
}


# full-archive-search about VACCINE or VAX in New York State 
# create a date sequence, from "2020-12-01" until "2021-08-31"
lt_dates <- seq(as.Date("2020-12-01"), as.Date("2021-08-31"), by = "days" )


# Search 2 - extract "vax" tweets by date and save as .json
idx = "search_idx"  

df <- data.frame(
  "date" = NA,
  "twt_count_S2"=NA
)
df$date <- as.Date(df$date)

for (i in 1:length(lt_dates) ) {
  tt <- request_tweet_vax(lt_dates[[i]], lt_dates[[i]])
  df[i,]$date <- paste(lt_dates[[i]] )
  df[i,]$twt_count_S2 <- nrow(tt$data)
  write_json(tt$data, paste("collected_twts/",idx,"-tweet-",lt_dates[[i]],".json", sep = ""), pretty = TRUE)
  write_json(tt$includes$users, paste("collected_twts/",idx,"-user-",lt_dates[[i]],".json", sep = ""), pretty = TRUE)
  write_json(tt$includes$places, paste("collected_twts/",idx,"-place-",lt_dates[[i]],".json", sep = ""), pretty = TRUE)
  
  Sys.sleep(2)   # stop for 2 secs 
}
write.csv(df, paste("collected_twts/00-",idx,"daily_tweets.csv", sep = ""), row.names = FALSE)


# search twts by conversation ID 
# extract unique conversation id 
tt <- gf_tweet[c("conversation_id", "created_at")]
tt$created_at <- as.Date(tt$created_at)
tt <- aggregate(created_at~conversation_id, tt, FUN = min)
tt$start_date <- tt$created_at - 3
gf_conv <- tt
gf_conv$n_twt <- NA
data <- gf_conv

# queries
for (j in 0:999) {
  a = 300*j + 1
  b = a + 299
  temp_twt <- data.frame()
  temp_place <- data.frame()
  
  for (i in a:b) {
    print(i)
    tt <- request_conversation(data$conversation_id[[i]], data$start_date[[i]])
    
    zz <- tt$data
    if(!is_null(zz)){
      zz <- do.call("data.frame", zz)
      data$n_twt[[i]] <- nrow(zz)
      
      if(length(zz$place_id) == 0){
        zz$place_id <- NA
      } else {
        mm <- tt$includes$places
        mm$geo$properties <- NA
        mm <- do.call("data.frame",mm)
        mm <- mm[c("id","full_name","geo.type","geo.bbox" )]
        temp_place <- rbind(temp_place, mm)
      }
    } else {
      data$n_twt[[i]] <- 0
    }
    
    zz <- zz[c("created_at","text","author_id","in_reply_to_user_id","conversation_id","public_metrics.retweet_count","public_metrics.reply_count","public_metrics.like_count","public_metrics.quote_count","id","lang","place_id")]
    temp_twt <- rbind(temp_twt, zz)
    
    Sys.sleep(1)   # stop for 1 secs 
  }
  
  write_json(temp_twt, paste("collected_twts/search_2-2/conversation/twt_", j,"-",a,"-", b,".json", sep = ""), pretty = TRUE)
  write_json(temp_place, paste("collected_twts/search_2-2/conversation/place_", j,"-",a,"-", b,".json", sep = ""), pretty = TRUE)
  
  Sys.sleep(600)   # stop for 2 secs
}

gf_conv <- data
