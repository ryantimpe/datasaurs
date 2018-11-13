#####
# Datasaurs 1.0
# Reboot Playground
#####

library(tidyverse)
library(twitteR)

# setwd("C:\Users\Ryan\Documents\808Projects\Datasaurs")

cod_all <- readRDS("BotInputs/D1_CauseOfDeath.RDS")
dino_info <- read.csv("BotInputs/DatasaurList.csv", stringsAsFactors = F)

source("D1_Functions.R")
source("D1_ColorsPatterns.R")

#Datasaur function will go here...

# Set up Twitter API
api_keys <- read.csv("BotInputs/API.csv", stringsAsFactors = FALSE)

setup_twitter_oauth(consumer_key = api_keys$consumer_key,
                    consumer_secret = api_keys$consumer_secret,
                    access_token = api_keys$access_token,
                    access_secret = api_keys$access_secret)


#Check for a special tweet count and set a pattern for that...
# I'd like this to be in the choose_pattern() function, but probably should keep API stuff here
tweet_data <- getUser("Datasaurs")
#2 non-datasaur tweets, +1 for this tweet
next_tweet_number <- (tweet_data$getStatusesCount() - 3 + 1) 

dino <- if(months.Date(Sys.Date()) == "July" && lubridate::day(Sys.Date()) >= 25 && lubridate::day(Sys.Date()) <= 28){
  dino_info %>% 
    filter(Family == "Shark") %>% 
    pull(Fauna) %>% 
    sample(1)
} else if(Sys.Date() %in% c("2018-11-22", "2019-11-28", "2020-11-26")){
  dino <- "Meleagris"
} else {
  sample(dino_info$Fauna, 1)
}

datasaur <- dino %>% 
  naked_datasaur() %>% 
  skin_datasaur(next_tweet_number %>% choose_pattern(col1_set = "Red", col2_set = "Gold")) %>% 
  wiki_datasaur() %>% 
  plot_datasaur() %>% 
  text_datasaur()

###
#TWEET IT!
###

updateStatus(datasaur$twitter_text, mediaPath = datasaur$filename, 
             bypassCharLimit=T)

