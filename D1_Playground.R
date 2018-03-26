#####
# Datasaurs 1.0
# Reboot Playground
#####

library(tidyverse);

cod_all <- readRDS("BotInputs/D1_CauseOfDeath.RDS")

dino_info <- read.csv("BotInputs/DatasaurList.csv", stringsAsFactors = F)

source("D1_Functions.R")
source("D1_ColorsPatterns.R")

#Datasaur function will go here...

dino_name <- sample(dino_info$Fauna, 1)

test_saur <- dino_name %>% 
  naked_datasaur() %>% 
  skin_datasaur(choose_pattern(col1_set = "Blue", col2_set = "Gold")) 

skin_datasaur0 <- test_saur
skin_datasaur0$color

test_saur %>% 
  plot_datasaur()


View(test_saur2$skin_datasaur)

