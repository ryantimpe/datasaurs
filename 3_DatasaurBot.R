#####
# Dino8
# Dino Charts
#####

library(png);library(tidyverse);
library(lubridate);
library(scales); library(zoo);
library(twitteR);

setwd("F:\\__RT Docs\\Dino8")

source("1_DatasaurFunction.R")

cod_all <- readRDS("BotInputs/CauseOfDeath.RDS")
greens <- read.csv("BotInputs/Greens.csv", stringsAsFactors = F)
greens <- greens$Greens

dino_info <- read.csv("BotInputs/DatasaurList.csv", stringsAsFactors = F)

###
# Make a datasaur
###

dino_list <- gsub(".png", "", list.files("PhyloPic/"))
dino_name <- sample(dino_list, 1)

datasaur_run <- datasaur(dino_name)

####
# Tweet it
####

# Set up Twitter API
api_keys <- read.csv("BotInputs/API.csv", stringsAsFactors = FALSE)

setup_twitter_oauth(consumer_key = api_keys$consumer_key,
                    consumer_secret = api_keys$consumer_secret,
                    access_token = api_keys$access_token,
                    access_secret = api_keys$access_secret)

#Save Datasaur
datasaur_filepath <- paste0("BotRuns/v0p1 ", substr(Sys.time(), 1, 13),".png")
ggsave(filename = datasaur_filepath, plot = datasaur_run[[1]])

datasaur_text <- paste0(dino_name, ": ", round(datasaur_run[[4]], 2), " correlation with US deaths from ", datasaur_run[[2]], " (", datasaur_run[[3]], ")")

#Clean up text a big
datasaur_text <- gsub(", not elsewhere classified", "", datasaur_text)
datasaur_text <- gsub(", including symptomatic,", "", datasaur_text)
datasaur_text <- gsub(" and certain disorders involving the immune mechanism", "", datasaur_text)

if(nchar(datasaur_text) > 135){
  datasaur_text <- substr(datasaur_text, 1, 135)
}
#Add hashtags sometimes
if(nchar(datasaur_text) < 125){
  dname <- paste0("#", strsplit(dino_name, " ")[[1]])
  dname <- ifelse(nchar(dname) < 15, dname, "")
  
  dino_hashes <- c(rep("#rstats", 5), 
                   rep("#dinosaurs", 5), rep("#dinos", 2), 
                   rep("#dataviz", 11), rep("#ggplot", 3),
                   rep("#correlation", 2), rep("#causation", 2),
                   rep("#science", 2), 
                   rep("", 4), rep(dname, 1))
  
  datasaur_text <- paste(datasaur_text, sample(dino_hashes, 1))
}

updateStatus(datasaur_text, mediaPath = datasaur_filepath, 
             bypassCharLimit=T)



