#####
# Dino8
# Dino Charts
#####

library(png);library(tidyverse);
library(lubridate);
library(scales); library(zoo);
library(twitteR);

setwd("C:\Users\Ryan\Documents\808Projects\Datasaurs")

source("1_DatasaurFunction.R")

cod_all <- readRDS("BotInputs/CauseOfDeath.RDS")
colorChoices <- read.csv("BotInputs/Colors.csv", stringsAsFactors = F)

dino_info <- read.csv("BotInputs/DatasaurList.csv", stringsAsFactors = F)

###
# Make a datasaur
###

#Choose the animal
dino_list <- gsub(".png", "", list.files("PhyloPic/"))
dino_name <- sample(dino_list, 1)

#Color! Default is green on green.
col1 <- "Green"
col2 <- "Green"

#Pattern! Default is spotted
pattern_list <- c("spotted", "striped", "geometric")
pattern <- sample(pattern_list, 1, prob = c(0.6, 0.25, 0.15))

#If December, allow for Holidatasaurs!
holidatasaur <- FALSE
if(months.Date(Sys.Date()) == "December"){
  holidatasaur <- sample(c(TRUE, FALSE), 1, prob = c(0.5, 0.5))
  if(holidatasaur){
    col2 <- "Red"
    pattern <- sample(pattern_list, 1, prob = c(0.1, 0.6, 0.3))
  }
}

datasaur_run <- datasaur(dino_name, col1 = col1, col2 = col2, pattern = pattern)

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
datasaur_filepath <- paste0("BotRuns/v0p2 ", substr(Sys.time(), 1, 13),".png")
ggsave(filename = datasaur_filepath, plot = datasaur_run[[1]])

datasaur_text <- paste0(dino_name, ": ", round(datasaur_run[[4]], 2), " correlation with US deaths from ", tolower(datasaur_run[[2]]), " (", datasaur_run[[3]], ")")

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
  
  dino_hashes <- c(rep("#rstats", 1), 
                   rep("#dinosaurs", 5), rep("#dinos", 2), 
                   rep("#dataviz", 10), rep("#dataisbeautiful", 2), 
                   rep("#ggplot", 1),
                   rep("#science", 3),
                   rep(dname, 6))
  
  if(weekdays(Sys.Date()) == "Friday"){
    dino_hashes <- c(rep("#FossilFriday", 100), #Very High Chance of #FossilFriday 
                     dino_hashes)
  }
  
  if(months.Date(Sys.Date()) == "November"){
    dino_hashes <- c(rep("#Dinovember", 50), #High chance of #Dinovember
                     dino_hashes)
  }
  
  datasaur_text <- paste(datasaur_text, 
                         sample(dino_hashes, 1),
                         if(holidatasaur){"#Holidatasaur"}) #SHould be fine bc have more than 140 char now
}

updateStatus(datasaur_text, mediaPath = datasaur_filepath, 
             bypassCharLimit=T)



