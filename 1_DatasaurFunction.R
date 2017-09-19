#####
# Dino8
# Dino Charts
#####

library(png);library(tidyverse);
library(lubridate)
library(scales)
library(zoo)

cod_all <- readRDS("BotInputs/CauseOfDeath.RDS")
greens <- read.csv("BotInputs/Greens.csv", stringsAsFactors = F)
greens <- greens$Greens

dino_info <- read.csv("BotInputs/DatasaurList.csv", stringsAsFactors = F)

#Text wrapping function
wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}

###
# Datasaur Function
###
datasaur <- function(dino_name){
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
  invert <- sample(c(TRUE, FALSE, FALSE), 1)
  if(invert){
    dino_long$x <- max(dino_long$x, na.rm=T) - dino_long$x + 1
  }
  
  #Upper outline of dino drawing
  dino_line <- dino_long %>% 
    arrange(x) %>% 
    group_by(x) %>% 
    filter(y == max(y)) %>% 
    select(-value) %>% 
    ungroup() %>% 
    rename(value_act = y) %>% 
    #Assign the line a time series start date similar to Cause of Death data
    mutate(Week = lubridate::ymd(paste0(sample(1997:2002, 1), "-", sample(1:12, 1), "-01")),
           #Week = lubridate::ymd("1997-01-01"), #For testing
           Week = Week+ 7*(row_number()-1)) %>%  
    mutate(Year = year(Week), Month = month(Week))
  
  ####
  # Corrs
  ###
  
  #Find 10 most correlated CoD series
  #Sample 1 of them
  corrs <- dino_line %>% 
    group_by(Year, Month) %>% 
    summarize(value_act = mean(value_act, na.rm=T)) %>% 
    ungroup() %>% 
    left_join(cod_all) %>% 
    arrange(Series, Year, Month) %>% 
    group_by(Series, Detail) %>% 
    summarize(cor = cor(value_act, value_cod, use="pairwise.complete.obs")) %>% 
    ungroup() %>% 
    filter(cor >= (max(cor, na.rm=T)/1.5)) %>% 
    arrange(desc(cor)) %>% 
    top_n(10, cor) %>% 
    sample_n(1)
  
  #Both lines for charting
  dino_cor <- dino_line %>% 
    left_join(cod_all %>% filter(Series == as.character(corrs[1, "Series"]),
                                 Detail == as.character(corrs[1, "Detail"]))) %>% 
    #Want the CoD time series to be on same scale as dino line
    mutate(value_cod_scaled = rescale(value_cod, c(min(value_act, na.rm=T)+25, max(value_act, na.rm=T)+25))) %>% 
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
    mutate(value_cod2 = zoo::rollmean(value_cod, 50, fill=NA)) %>% 
    mutate(value_cod = ifelse(is.na(value_cod2), value_cod, value_cod2)) %>% 
    select(-value_cod2)
  
  #Raise the CoD line if it's below the minimum y of the dino for any x
  dino_silho <- dino_long %>% 
    arrange(x) %>% 
    left_join(dino_rejig) %>% 
    mutate(first = x[min(which(!is.na(value_cod)))],
           last = x[max(which(!is.na(value_cod)))]) %>% 
    fill(value_cod) %>% 
    mutate(value_cod = ifelse(x >= first & x <= last, value_cod, value_act)) %>% 
    group_by(x) %>% 
    mutate(check = any(value_cod < min(y))) %>% 
    ungroup() %>% 
    mutate(adjust = ifelse(check, (y - value_cod)+25, NA)) %>% 
    group_by(x) %>% 
    mutate(adjust = ifelse(check, min(adjust, na.rm=T), 0)) %>% 
    ungroup() %>% 
    mutate(adjust = ifelse(any(check), max(adjust, na.rm=T), 0)) %>% 
    rowwise() %>% 
    mutate(y_cod = ifelse(x >= first & x <= last, round(value_cod + adjust), value_cod)) %>% 
    ungroup() %>% 
    select(x, y, y_cod, first, last, adjust) %>% 
    group_by(x) %>% 
    mutate(min_dino = min(y)) %>% 
    ungroup()
  
  dino_silho_cod <- dino_silho %>% 
    select(-y) %>% 
    distinct()
  
  dino_silho_cod2 <- lapply(1:nrow(dino_silho_cod), function(i){
    y.min <- as.numeric(dino_silho_cod[i, "min_dino"])
    y.max <- as.numeric(dino_silho_cod[i, "y_cod"])
    data.frame(x = i, y = y.max:y.min)
  })
  dino_silho_cod2 <- bind_rows(dino_silho_cod2) %>% 
    mutate(Line = "value_cod")
    
  #Build new dino....
  #Try to retain as much of the original body as possible
  #This is tough since it's hard to distinguish between fins/scales/spikes/sails (drop them)
  #... and legs/arms/toes/tails (keep them)
  dino_silho2 <- dino_silho %>% 
    select(x, y) %>% 
    mutate(Line = "value_act") %>% 
    bind_rows(dino_silho_cod2) %>% 
    group_by(x, Line) %>% 
    mutate(Num = row_number()) %>% 
    ungroup() %>% 
    spread(Line, y) %>% 
    select(-Num) %>% 
    #Find disjoints in the dino
    group_by(x) %>%
    mutate(delta_dino = value_act - lag(value_act)) %>%
    mutate(Body_segments = ifelse(is.na(delta_dino) | delta_dino == -1, 0, 1)) %>% 
    mutate(Body_segments = cumsum(Body_segments)) %>% 
    ungroup() %>% 
    group_by(x, Body_segments) %>% 
    mutate(Body_seg_max = n() - sum(is.na(value_act))) %>% 
    ungroup() %>% 
    group_by(x) %>%
    mutate(Body_seg_check = ifelse(Body_seg_max >= 25 | Body_seg_max== max(Body_seg_max, na.rm=T), Body_seg_max, 0)) %>% 
    mutate(Body_seg_check2 = ifelse(Body_seg_check == Body_seg_check[min(which(Body_seg_check >0))], Body_seg_check, 0)) %>% 
    mutate(Body_seg_y = value_act[max(which(Body_seg_check2 == max(Body_seg_check2, na.rm=T)), na.rm=T)],
           Body_seg_y = ifelse(is.na(Body_seg_y), 0, Body_seg_y)) %>% 
    ungroup() %>%
    mutate(value_cod = ifelse(value_cod > max(value_act, na.rm=T), value_cod, 
                              ifelse(x < as.numeric(dino_silho[1, "first"]) | x > as.numeric(dino_silho[1, "last"]), value_act, value_cod))) %>% 
    #Drop appendages
    group_by(x) %>%
    mutate(value_cod = ifelse((value_cod < Body_seg_y &
                                 !(value_cod %in% value_act)),
                              NA, value_cod)) %>%
    ungroup() %>%
    gather(Line, y, value_act, value_cod) %>% 
    distinct() %>% 
    filter(!is.na(y)) %>% 
    mutate(Chart = ifelse(Line == "value_act", " Original", "Datasaur"))
  
  #Minimum Y for additional overwrites
  min_cod_y <- dino_cor %>% 
    filter(Line == "value_cod")
  min_cod_y <- min(min_cod_y$value, na.rm=T) - max(25, as.numeric(dino_silho[1, "adjust"]))
    
  dino_silho3 <- dino_silho2 %>% 
    filter(!(Line == "value_cod" & y < min_cod_y)) %>% 
    bind_rows(dino_silho2 %>%
                filter((Line == "value_act" & y < min_cod_y)) %>%
                mutate(Line = "value_cod", Chart = "Datasaur")) %>%
    arrange(x, Line, desc(y))

  #Place holder for additional edits
  dino_cor2 <- dino_cor 
  
  ###
  #PLOT!
  ###

  sel_green <- sample(greens, 1) #I like #108070 malachite

  #Annual X labels... 
  #TODO: clean this
  xlabs <- dino_cor %>% 
    filter(Line == "value_act") %>% 
    mutate(YM = ifelse(row_number() %% 52 != 0, NA, paste0(Year, " M", Month))) %>% 
    select(x, YM) %>% 
    filter(!is.na(YM))
  
  chart <- ggplot(dino_cor2, aes(x = x, y = value, group=Line, color=Line)) + 
    geom_raster(data=dino_silho3, aes(x=x, y=y, fill=Chart))+
    geom_line(size = 1.5) +
    scale_color_manual(values = c("value_cod" = "#FC3D32", "value_act" = sel_green)) +
    scale_fill_manual(values = c(" Original" = "#CCCCCC", "Datasaur" = sel_green)) +
    coord_equal() +
    facet_grid(Chart ~.) +
    scale_y_continuous(limits=c(0, NA), breaks = NULL, 
                       name = paste0("US Cause of Death:", "\n", 
                                     wrapper(as.character(corrs[1, "Series"]), 60), "\n",
                                    " (", as.character(corrs[1, "Detail"]), ")")) +
    scale_x_continuous(labels = xlabs$YM, breaks = xlabs$x, name = NULL) +
    labs(title = paste0(dino_name),
         caption = paste(dino_name, "by", as.character(info$Credit[1]), 
                         "| Cause of death data from CDC.gov", "\n", "@Datasaurs v0.1.1")) +
    theme_minimal()+
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_rect(fill = "azure4", color="white"),
          strip.text = element_text(color="white", size = 12),
          axis.text.x = element_text(angle = 45),
          axis.title.y = element_text(size = 14, color="#DA1B10"),
          plot.title = element_text(size = 20, face="bold.italic")
          )
  
  ###
  #Return datasaur
  ###
  datasaur.list <- list()
  datasaur.list[["chart"]] <- chart
  datasaur.list[["data_series"]] <- as.character(corrs[1, "Series"])
  datasaur.list[["data_detail"]] <- as.character(corrs[1, "Detail"])
  datasaur.list[["cor"]] <- as.numeric(corrs$cor[1])
  
  return(datasaur.list)
  
}

