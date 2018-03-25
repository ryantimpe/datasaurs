#####
# Datasaurs 1.0
# Reboot Playground
#####

library(png);library(tidyverse);
library(lubridate)
library(scales)
library(zoo)

cod_all <- readRDS("BotInputs/D1_CauseOfDeath.RDS")
colorChoices <- read.csv("BotInputs/Colors.csv", stringsAsFactors = F)

dino_info <- read.csv("BotInputs/DatasaurList.csv", stringsAsFactors = F)

#Text wrapping function
wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}


#Datasaur function will go here...

dino_name <- "Allosaurus"

dino_raw <- readPNG(paste0("PhyloPic/", dino_name,".png"))

dino <- dino_raw[, , 4] #Only need the transparency matrix

#Get fauna artist
info <- dino_info %>% 
  filter(Fauna == dino_name)

#Convert matrix to tidy data frame. 
dino_long <- as.data.frame(dino) %>% 
  mutate(y = n() - row_number()) %>% 
  gather(x, value, starts_with("V")) %>% 
  mutate(x = as.numeric(substr(x, 2, 10))) %>% 
  filter(value > 0.5) #clean up noise, retaining only more that 50% transparent cells

#Boolean to invert the drawing along x axis
if(sample(c(TRUE, FALSE), 1, prob = c(1, 1))){
  dino_long$x <- max(dino_long$x, na.rm=T) - dino_long$x + 1
}

#Upper outline of dino drawing
# Create a weekly "timeseries" rather than X values
dino_line <- dino_long %>% 
  arrange(x) %>% 
  group_by(x) %>% 
  filter(y == max(y)) %>% 
  select(-value) %>% 
  ungroup() %>% 
  rename(value_act = y) %>% 
  #Assign the line a time series start date similar to Cause of Death data
  mutate(Week = lubridate::ymd(paste0(sample(1997:2002, 1), "-", sample(1:12, 1), "-01")),
         Week = Week + 7*(row_number()-1)) %>%  
  mutate(Year = year(Week), Month = month(Week))

####
# Corrs
###

#Find 10 most correlated CoD series
corrs_ss <- sample(1:4, 1, prob = (1/(1:4))^(2))

corrs <- dino_line %>% 
  group_by(Year, Month) %>% 
  summarize(value_act = mean(value_act, na.rm=T)) %>% 
  ungroup() %>% 
  left_join(cod_all %>% unnest()) %>% 
  arrange(Series, Year, Month) %>% 
  group_by(Source, Series_Type, Series, Detail) %>% 
  summarize(cor = cor(value_act, value_cod, use="pairwise.complete.obs")) %>% 
  ungroup() %>% 
  #Keep those with minimum correlation
  filter(cor >= (max(cor, na.rm=T)/1.5)) %>% 
  arrange(desc(cor)) %>% 
  #Sampel the N corrs
  sample_n(corrs_ss) %>% 
  #But if any repeated series, only keep one
  group_by(Series) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  mutate(id = paste(Source, Series_Type, Series, Detail))

cod_corr <- cod_all %>% 
  mutate(id = paste(Source, Series_Type, Series, Detail)) %>% 
  filter(id %in% corrs$id) %>% 
  select(-id) %>% 
  unnest() %>% 
  do(
    if(nrow(corrs) > 1){
      mutate(., Source = "Multiple", Series_Type = "Multiple", Series = "Multiple causes", Detail = "Varied")
    } else {.}
  ) %>% 
  group_by_(.dots = names(.)[!(names(.) %in% c("value_cod"))]) %>% 
  summarise(value_cod = mean(value_cod, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(value_cod = ifelse(is.nan(value_cod), NA, value_cod))

#Both lines for charting
dino_cor <- dino_line %>% 
  left_join(cod_corr) %>% 
  #Want the CoD time series to be on same scale as dino line
  # But only for x where there is both a dino and a CoD line
  mutate(value_cod_scaled = rescale(value_cod, c(min(value_act[!is.na(.$value_cod)], na.rm=T)+25,
                                                 max(value_act[!is.na(.$value_cod)], na.rm=T)+25))) %>% 
  select(-value_cod) %>% 
  rename(value_cod = value_cod_scaled) %>% 
  group_by(Year, Month) %>% 
  mutate(value_cod = ifelse(Week == first(Week), value_cod, NA)) %>% 
  ungroup() %>% 
  gather(Line, value, value_act, value_cod) %>% 
  filter(!is.na(value)) %>% 
  mutate(Chart = " Original")


###
#Create new silhouette of dino
#This part is sloppy
###

#Smooth the monthly times series
dino_rejig <- dino_cor %>% 
  spread(Line, value) %>% 
  select(x, value_act, value_cod) %>% 
  fill(value_cod) %>% 
  mutate(value_cod2 = zoo::rollmean(value_cod, 20, fill=NA)) %>% 
  mutate(value_cod = ifelse(is.na(value_cod2), value_cod, value_cod2)) %>% 
  select(-value_cod2)

#Rotate the Datasaur so the body hits both the start and end of line
dino_silho <- dino_long %>% 
  arrange(x, desc(y)) %>% 
  left_join(dino_rejig) %>% 
  select(-value) %>% 
  mutate(first = x[min(which(!is.na(value_cod)))],
         last = x[max(which(!is.na(value_cod)))]) %>% 
  fill(value_cod) %>% 
  mutate(value_cod = ifelse(x >= first & x <= last, value_cod, value_act)) %>% 
  #Deltas for twisting first and last
  mutate(twist_front = ifelse(x == first, value_cod - value_act, NA),
         twist_front = max(twist_front, na.rm=TRUE),
         twist_back  = ifelse(x == last, value_cod - value_act, NA),
         twist_back  = max(twist_back, na.rm=TRUE)) %>% 
  mutate(share_back = (x-1) / (last - first)) %>% 
  mutate(twist = (1-share_back)*twist_front + (share_back)*twist_back)



