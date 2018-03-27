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

datasaur <- sample(dino_info$Fauna, 1) %>% 
  naked_datasaur() %>% 
  skin_datasaur(choose_pattern()) %>% 
  plot_datasaur() %>% 
  text_datasaur()

###
#TWEET IT!
###

# Set up Twitter API
api_keys <- read.csv("BotInputs/API.csv", stringsAsFactors = FALSE)

setup_twitter_oauth(consumer_key = api_keys$consumer_key,
                    consumer_secret = api_keys$consumer_secret,
                    access_token = api_keys$access_token,
                    access_secret = api_keys$access_secret)

updateStatus(datasaur$twitter_text, mediaPath = datasaur$filename, 
             bypassCharLimit=T)

