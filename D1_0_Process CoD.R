#####
# Dino8
# Cause of Death
#####

library(tidyverse);
library(lubridate)

#Source: 
#https://wonder.cdc.gov/ucd-icd10.html

cod_list <- list()
cod_files <- list.files("CoD/")
cod_files <- cod_files[!grepl("map_", cod_files, fixed = TRUE)]

map_cod <- read.csv("CoD/Map_cod.csv", stringsAsFactors = F)

for(file in cod_files){
  cod_raw <- read_delim(paste0("CoD/", file), "\t", escape_double = FALSE, trim_ws = TRUE)
  
  paren <- gsub("[\\(\\)]", "", regmatches(file, gregexpr("\\(.*?\\)", file))[[1]])
  
  names(cod_raw)[names(cod_raw) == "ICD Sub-Chapter"] <- "Series"
  names(cod_raw)[names(cod_raw) == "ICD Chapter"] <- "Series"
  
  cod_1 <- cod_raw %>% 
    select(Series, Year, Month = `Month Code`, value = Deaths) %>% 
    filter(!is.na(Series), !is.na(Month)) %>% 
    mutate(Detail = paren) %>% 
    mutate(Month = as.numeric(substr(Month, 6, 7))) %>% 
    group_by(Series) %>% 
    filter(mean(value, na.rm=T) >= 6) %>% #Want a mininum of 6deaths on average
    mutate(value = (value + lag(value, 1) + lag(value, 2) + lag(value, 3) + lag(value, 4) + lag(value, 5))/6) %>% 
    filter(n() >= 24) %>% #Need min # of observations
    rename(value_cod = value) %>% 
    ungroup() %>% 
    filter(!is.na(Year))
  
  cod_list[[paren]] <- cod_1
  
}

cod_all <- bind_rows(cod_list) %>% 
  nest(Year, Month, value_cod) %>% 
  mutate(Source = "CDC.gov",
         Series_Type = "Cause of death") %>% 
  #Map Series names
  rename(Series_raw = Series) %>% 
  left_join(map_cod) %>% 
  select(-Series_raw) %>% 
  select(Source, Series_Type, Series, everything())


saveRDS(cod_all, "BotInputs/D1_CauseOfDeath.RDS")

