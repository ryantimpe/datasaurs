# Choose Pattern / Colors
choose_pattern <- function(tweet_num, col1_set = NULL, col2_set = NULL, 
                           pattern_set = NULL){
  
  #Get list of allowed color categories and color hexes
  colorChoices <- read.csv("BotInputs/Colors.csv", stringsAsFactors = F)
  col_list <- unique(colorChoices$Category)

  ###
  #Choose color types  
  ### 
  #Default is green on green. Sometimes surprises.
  
  #Col 1
  if(!is.null(col1_set) && col1_set %in% col_list){
    col1 <- col1_set
  } else{
    col1 <- "Green"
  }
  #Col 2
  if(!is.null(col2_set) && col2_set %in% col_list){
    col2 <- col2_set
  } else{
    col2_list <- c("Green" = 95, "Blue" = 5, "Gold" = 3, "Dark" = 2)
    col2 <- sample(names(col2_list), 1, prob = col2_list)
  }
  
  ###
  #Pattern!
  ###

  pattern_list <- c("spotted" = 40, 
                    "striped" = 20, 
                    "geometric" = 15, 
                    "dotted" = 20, 
                    "3dotted" = 30,
                    "diamond" = 20,
                    "zebra" = 10,
                    "hearts" = 1, #Rare except valentines day,
                    "rainbow" = 0.1, #Super rare except for June... then less rare
                    "america" = 0.1, #Super rare except for US patriotic holidays
                    "celebrate" = 0.0001 #Super rare except valentines day,
  )
  
  #Every 500 images, use celebrate
  if(next_tweet_number %% 500 == 0){
    pattern_tweet <- "celebrate"
  } else { pattern_tweet <- pattern_set}
  
  if(!is.null(pattern_tweet) && pattern_tweet %in% names(pattern_list)){
    pattern <- pattern_tweet
  } else{
    pattern <- sample(names(pattern_list), 1, prob = pattern_list)
  }
  
  
  # DECEMBER --> Holidatasaurs! ----
  holidatasaur <- FALSE
  if(months.Date(Sys.Date()) == "December" && lubridate::day(Sys.Date()) < 29){
    holidatasaur <- sample(c(TRUE, FALSE), 1, prob = c(0.5, 0.5)) #50% chance in December
    if(holidatasaur){
      col2 <- "Red"
      
      pattern_list <- c("spotted" = 10, 
                        "striped" = 40, 
                        "geometric" = 10, 
                        "dotted" = 20, 
                        "3dotted" = 40,
                        "diamond" = 40,
                        "zebra" = 20,
                        "hearts" = 5,
                        "rainbow" = 0,
                        "america" = 0,
                        "celebrate" = 0
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
  if(as.character(Sys.Date()) %in% americasaur_dates){
    americasaur <- sample(c(TRUE, FALSE), 1, prob = c(1, 0.25)) #75% chance on US patriot holidays
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
                        "striped" = 40, 
                        "geometric" = 10, 
                        "dotted" = 25,
                        "3dotted" = 40,
                        "diamond" = 40,
                        "hearts" = 15,
                        "rainbow" = 0,
                        "america" = 0
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
  # JUNE --> PRIDE (Dark + Rainbow) ----
  pridesaur <- FALSE
  if((months.Date(Sys.Date()) == "June")){
    pridesaur <- sample(c(TRUE, FALSE), 1, prob = c(1, 9)) # Chance of Pride datasaur in June
    if(pridesaur){
      col1 <- "Dark"
      pattern <- "rainbow"
    }
  }
  
  #Return Pattern
  pattern_out <- list(
    col1 = col1, col2 = col2,
    pattern = pattern,
    next_tweet = next_tweet_number,
    holidatasaur = holidatasaur, americasaur = americasaur,
    newyearsaur = newyearsaur, valentinesaur = valentinesaur, 
    stpatrick = stpatrick, pridesaur = pridesaur
  )
  return(pattern_out)
}
