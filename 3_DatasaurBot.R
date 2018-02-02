#####
# Dino8
# Dino Charts
#####

library(png);library(tidyverse);
library(lubridate);
library(scales); library(zoo);
library(twitteR);

# setwd("C:\Users\Ryan\Documents\808Projects\Datasaurs")

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

#Color! Default is green on green. Sometimes surprises.
col1 <- "Green"
col2_list <- c("Green" = 95, "Blue" = 5, "Gold" = 3, "Dark" = 2)
col2 <- sample(names(col2_list), 1, prob = col2_list)

#Pattern!
pattern_list <- c("spotted" = 40, #For now, Spotted should always be ~50%... even when you get excited about adding a new pattern
                  "striped" = 20, 
                  "geometric" = 20, 
                  "dotted" = 20, 
                  "diamond" = 20,
                  "hearts" = 1, #Rare except valentines day,
                  "rainbow" = 0.1, #Super rare except for June... then less rare
                  "america" = 0.1 #Super rare except for US patriotic holidays
                  )
pattern <- sample(names(pattern_list), 1, prob = pattern_list)

# DECEMBER --> Holidatasaurs! ----
holidatasaur <- FALSE
if(months.Date(Sys.Date()) == "December" && lubridate::day(Sys.Date()) < 29){
  holidatasaur <- sample(c(TRUE, FALSE), 1, prob = c(0.5, 0.5)) #50% chance in December
  if(holidatasaur){
    col2 <- "Red"
    
    pattern_list <- c("spotted" = 5, 
                      "striped" = 25, 
                      "geometric" = 10, 
                      "dotted" = 5,
                      "diamond" = 5, 
                      "america" = 0
    )
    pattern <- sample(names(pattern_list), 1, prob = pattern_list)
  }
}


# PRESIDENTS DAY, FLAG DAY, 4TH OF JULY, VETERANS DAY --> Americasaur ----
americasaur_dates <- c("#PresidentsDay" = "2018-02-19", "#FlagDay" = "2018-06-14", 
                 "#IndependenceDay" = "2018-07-04", "#VeteransDay" = "2018-11-11",
                 "#PresidentsDay" = "2019-02-18", "#FlagDay" = "2019-06-14", 
                 "#IndependenceDay" = "2019-07-04", "#VeteransDay" = "2019-11-11"
                 )
americasaur <- FALSE
if(Sys.Date() %in% americasaur_dates){
  americasaur <- sample(c(TRUE, FALSE), 1, prob = c(0.75, 0.25)) #75% chance on US patriot holidays
  if(americasaur){pattern <- "america"}
}


# JANUARY / DECEMBER --> New Years datasaur ----
newyearsaur <- FALSE
if((months.Date(Sys.Date()) == "January"  && lubridate::day(Sys.Date()) == 1) ||
   (months.Date(Sys.Date()) == "December" && lubridate::day(Sys.Date()) == 31)){
  newyearsaur <- sample(c(TRUE, FALSE), 1, prob = c(0.75, 0.25)) #75% of new years colors
  if(newyearsaur){
    col1 <- "Dark"
    col2 <- "Gold"
    
    pattern_list <- c("spotted" = 20, 
                      "striped" = 20, 
                      "geometric" = 10, 
                      "dotted" = 25,
                      "diamond" = 15
    )
    pattern <- sample(names(pattern_list), 1, prob = pattern_list)
  }
}
# FEBRUARY 14 --> Valentines Day ----
valentinesaur <- FALSE
if((months.Date(Sys.Date()) == "February"  && lubridate::day(Sys.Date()) == 14)){
  valentinesaur <- sample(c(TRUE, FALSE), 1, prob = c(1, 0)) # 100% of valenties
  
  valentine_red <- sample(c("back", "fore"), 1, prob = c(1, 1))
  if(valentine_red == "back"){
    col1 <- "Red"
    
    col2_list <- c("Red" = 10, "Blue" = 2, "Gold" = 10, "Dark" = 5)
    col2 <- sample(names(col2_list), 1, prob = col2_list)
  } else {
    col1_list <- c("Green" = 10, "Gold" = 5, "Dark" = 5)
    col1 <- sample(names(col1_list), 1, prob = col1_list)
    
    col2 <- "Red"
  }
  
  pattern <- "hearts"
}


# MARCH 17 --> ST Patricks Day ----
stpatrick <- FALSE
if((months.Date(Sys.Date()) == "March" && lubridate::day(Sys.Date()) == 17)){
  stpatrick <- sample(c(TRUE, FALSE), 1, prob = c(1, 0)) #100%
  if(stpatrick){
    col1 <- "Green"
    pattern <- "rainbow"
  }
}
# JUNE --> ST Patricks Day ----
pridesaur <- FALSE
if((months.Date(Sys.Date()) == "June")){
  pridesaur <- sample(c(TRUE, FALSE), 1, prob = c(1, 9)) # Chance of Pride datasaur in June
  if(pridesaur){
    col1 <- "Dark"
    pattern <- "rainbow"
  }
}
#RUN ----
datasaur_run <- datasaur(dino_name, col1 = col1, col2 = col2, pattern = pattern)
datasaur_run

####
# Tweet it ---- 
####

# Set up Twitter API
api_keys <- read.csv("BotInputs/API.csv", stringsAsFactors = FALSE)

setup_twitter_oauth(consumer_key = api_keys$consumer_key,
                    consumer_secret = api_keys$consumer_secret,
                    access_token = api_keys$access_token,
                    access_secret = api_keys$access_secret)

#Save Datasaur
twit.width  <- 1024 #pixels
twit.height <-512 #pixels
twit.dpi <- 300

twit.width <- twit.width / twit.dpi
twit.height <- twit.height / twit.dpi

datasaur_filepath <- paste0("BotRuns/v0p2 ", substr(Sys.time(), 1, 13),".png")

ggsave(filename = datasaur_filepath, plot = datasaur_run[[1]],
       width = twit.width*3, height = twit.height*3)

datasaur_text <- paste0(dino_name, ": ", round(datasaur_run[[4]], 2), " correlation with US deaths from ", tolower(datasaur_run[[2]]), " (", datasaur_run[[3]], ")")

#Clean up text a big
datasaur_text <- gsub(", not elsewhere classified", "", datasaur_text)
datasaur_text <- gsub(", including symptomatic,", "", datasaur_text)
datasaur_text <- gsub(" symptoms, signs and", "", datasaur_text)
datasaur_text <- gsub(" and certain disorders involving the immune mechanism", "", datasaur_text)
datasaur_text <- gsub("hiv", "HIV", datasaur_text)

# if(nchar(datasaur_text) > 135){
#   datasaur_text <- substr(datasaur_text, 1, 135)
# }

#Add hashtags sometimes ----
if(nchar(datasaur_text) < 175){
  dname <- paste0("#", strsplit(dino_name, " ")[[1]])
  
  dino_hashes <- c("#rstats" = 2, 
                   #"#dinosaurs" = 5, "#dinos" = 2, 
                   "#dataviz" = 5, "#dataisbeautiful" = 2, 
                   "#machinelearning" = 3, "#datascience" = 4, "#analytics" = 1,
                   "#science" = 2, "#statistics" = 2,
                   "#paleontology" = 2, "#paleobiology" = 2, 
                   "#bioinformatics" = 3)
  
  dname_hash <- 6
  names(dname_hash) <- dname
  dino_hashes <- c(dino_hashes, dname_hash)
  
  if(weekdays(Sys.Date()) == "Friday"){
    dino_hashes <- c("#FossilFriday" = 80, #Very High Chance of #FossilFriday 
                     dino_hashes)
  }
  
  if(months.Date(Sys.Date()) == "November"){
    dino_hashes <- c("#Dinovember" = 50, #High chance of #Dinovember
                     dino_hashes)
  }
  
  datasaur_text <- paste(datasaur_text, 
                         sample(c(names(dino_hashes), ""), 1, prob = c(dino_hashes, 5)),
                         #Additional hashtags
                         if(holidatasaur){"#Holidatasaur"}, 
                         if(pattern == "america"){"#Americasaur"},
                         if(valentinesaur){"#Valentinesaur"},
                         if(stpatrick){"#StPaddatasaur"},
                         if(pridesaur){"#PRIDEsaur"},
                         if(newyearsaur){paste("#HappyNewYears", paste0("#NY", lubridate::year(Sys.Date()+1)))} # +1 to account for NYE
                         )
}

updateStatus(datasaur_text, mediaPath = datasaur_filepath, 
             bypassCharLimit=T)



