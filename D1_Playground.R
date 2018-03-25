#####
# Datasaurs 1.0
# Reboot Playground
#####

library(tidyverse);

cod_all <- readRDS("BotInputs/D1_CauseOfDeath.RDS")
colorChoices <- read.csv("BotInputs/Colors.csv", stringsAsFactors = F)

dino_info <- read.csv("BotInputs/DatasaurList.csv", stringsAsFactors = F)

source("D1_Functions.R")

#Datasaur function will go here...

dino_name <- sample(dino_info$Fauna, 1)

test_saur <- dino_name %>% naked_datasaur()

ggplot(test_saur$data, aes(x=x, y=y)) + 
  geom_raster(fill = "#00436b") +
  coord_fixed() + 
  facet_grid(Chart ~ .)

