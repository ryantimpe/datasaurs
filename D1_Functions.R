library(png);library(tidyverse);
library(lubridate)
library(scales)
library(zoo)
library(grid); library(gridExtra)

#Text wrapping function
wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}

#Produce Dataframe of datasaur raster points
naked_datasaur_deprecated <- function(dino_name){
  
  dino_raw <- readPNG(paste0("PhyloPic/", dino_name,".png"))
  
  dino <- dino_raw[, , 4] #Only need the transparency matrix
  
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
  # corrs_ss <- sample(1:4, 1, prob = (1/(1:4))^(2))
  corrs_ss <- 1
  
  corrs <- dino_line %>% 
    group_by(Year, Month) %>% 
    summarize(value_act = mean(value_act, na.rm=T)) %>% 
    ungroup() %>% 
    left_join(cod_all %>% unnest(), by = c("Year", "Month")) %>% 
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
    left_join(cod_corr, by = c("Year", "Month")) %>% 
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
    #Fill existing months
    group_by(Year, Month) %>% 
    fill(value_cod) %>% 
    ungroup() %>% 
    #Keep only needed
    select(x, value_act, value_cod) %>%
    mutate(value_cod2 = zoo::rollmean(value_cod, 20, fill=NA)) %>% 
    mutate(value_cod = ifelse(is.na(value_cod2), value_cod, value_cod2)) %>% 
    select(-value_cod2) %>% 
    #Slope the cut-off
    mutate(end_start = x[min(which(is.na(value_cod) & x > max(x, na.rm=TRUE)/2))],
           end_end = x[max(which(is.na(value_cod)))],
           end_end = end_start + (1/3)*(end_end - end_start)) %>%
    mutate(slope = value_cod) %>%
    fill(slope) %>%
    mutate(slope = ifelse(!is.na(value_cod) & x < end_start, NA,
                          slope*((end_end - x) / (end_end - end_start))^(1/2))) %>%
    mutate(value_cod = ifelse(!is.na(value_cod), value_cod,
      ifelse(slope > value_act, slope, NA))) %>%
    select(-end_start, -end_end, -slope) %>%
    #Slope the cut-off
    arrange(desc(x)) %>%
    mutate(x = max(x) - x + 1) %>%
    mutate(end_start = x[min(which(is.na(value_cod) && x > max(x, na.rm=TRUE)/2))],
           end_end = x[max(which(is.na(value_cod)))],
           end_end = end_start + (1/3)*(end_end - end_start)) %>%
    mutate(slope = value_cod) %>%
    fill(slope) %>%
    mutate(slope = ifelse(!is.na(value_cod) & x < end_start, NA,
                          slope*((end_end - x) / (end_end - end_start))^(1/2))) %>%
    mutate(value_cod = ifelse(!is.na(value_cod), value_cod,
                              ifelse(slope > value_act, slope, NA))) %>%
    select(-end_start, -end_end, -slope) %>%
    arrange(desc(x)) %>%
    mutate(x = max(x) - x + 1) %>%
    #SMooth more
    mutate(value_cod2 = (lag(value_cod, 2) + lag(value_cod) + 
                           value_cod*2 + 
                           lead(value_cod) + lead(value_cod, 2))/6) %>% 
    mutate(value_cod = ifelse(is.na(value_cod2), value_cod, value_cod2)) %>% 
    select(-value_cod2)
  
  #Raise the CoD line if it's below the minimum y of the dino for any x
  dino_silho <- dino_long %>% 
    select(-value) %>% 
    arrange(x) %>% 
    left_join(dino_rejig, by = c("x")) %>% 
    filter(!is.na(value_act)) %>% 
    mutate(first = x[min(which(!is.na(value_cod)))],
           last = x[max(which(!is.na(value_cod)))]) %>% 
    fill(value_cod) %>% 
    mutate(value_cod = ifelse(x >= first & x <= last, value_cod, value_act)) %>% 
    group_by(x) %>% 
    mutate(check = any(value_cod < min(y))) %>% 
    ungroup() %>% 
    mutate(adjust = ifelse(check, (y - value_cod)+15, NA)) %>% 
    group_by(x) %>% 
    mutate(adjust = ifelse(check, min(adjust, na.rm=T), 0)) %>% 
    ungroup() %>% 
    mutate(adjust = ifelse(any(check), max(adjust, na.rm=T), 0),
           adjust = ifelse(is.na(adjust), 0, adjust)) %>%
    rowwise() %>% 
    mutate(y_cod = ifelse(x >= first & x <= last, floor(value_cod + adjust), value_cod)) %>% 
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
    mutate(value_cod = ifelse(value_cod > max(value_act, na.rm=T), floor(value_cod), 
                              ifelse(x < as.numeric(dino_silho[1, "first"]) | x > as.numeric(dino_silho[1, "last"]), 
                                     value_act, floor(value_cod)))) %>% 
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
    group_by(x %/% 200) %>% 
    mutate(min_cod_y_value = min(value)) %>% 
    ungroup() %>% 
    select(Line, x, min_cod_y_value) %>% 
    mutate(min_cod_y_value = floor(min_cod_y_value - 25 + as.numeric(dino_silho[1, "adjust"])))
  
  dino_silho3 <- dino_silho2 %>% 
    left_join(min_cod_y, by = c("Line", "x")) %>% 
    filter(!(Line == "value_cod" && y < min_cod_y_value) || is.na(min_cod_y_value)) %>% 
    bind_rows(dino_silho2 %>%
                left_join(min_cod_y, by = c("Line", "x")) %>% 
                filter((Line == "value_act" && y < min_cod_y_value) || is.na(min_cod_y_value)) %>%
                mutate(Line = "value_cod", Chart = "Datasaur")) %>%
    mutate(y = floor(y)) %>% 
    arrange(x, Line, desc(y))
  
  
  #Minimum Y for additional overwrites
  min_cod_y2 <- dino_cor %>%
    filter(Line == "value_cod")
  min_cod_y2 <- floor(min(min_cod_y2$value, na.rm=T) - 25 + 
                        as.numeric(dino_silho[1, "adjust"])/2)
  
  dino_silho3 <- dino_silho3 %>%
    filter(!(Line == "value_cod" & y < min_cod_y2)) %>%
    bind_rows(dino_silho2 %>%
                filter((Line == "value_act" & y < min_cod_y2)) %>%
                mutate(Line = "value_cod", Chart = "Datasaur")) %>%
    arrange(x, Line, desc(y))
  
  
  #RETURN
  dino_silho_out <- dino_silho3 %>% 
    select(Chart, Line, x, y)

  out.list <- list(
    datasaur_name = dino_name,
    corrs = corrs,
    line_chart_data = dino_cor,
    naked_datasaur = dino_silho_out
  )
  
  return(out.list)
}

#Produce Dataframe of datasaur raster points
# NEW EDIT. Probably got rid of the glitches. A bit slower though.
naked_datasaur <- function(dino_name){
  
  dino_raw <- readPNG(paste0("PhyloPic/", dino_name,".png"))
  
  dino <- dino_raw[, , 4] #Only need the transparency matrix
  
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
  # corrs_ss <- sample(1:4, 1, prob = (1/(1:4))^(2))
  corrs_ss <- 1
  
  corrs <- dino_line %>% 
    group_by(Year, Month) %>% 
    summarize(value_act = mean(value_act, na.rm=T)) %>% 
    ungroup() %>% 
    left_join(cod_all %>% unnest(), by = c("Year", "Month")) %>% 
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
    left_join(cod_corr, by = c("Year", "Month")) %>% 
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
    #Fill existing months
    group_by(Year, Month) %>% 
    fill(value_cod) %>% 
    ungroup() %>% 
    #Keep only needed
    select(x, value_act, value_cod) %>%
    mutate(value_cod2 = zoo::rollmean(value_cod, 20, fill=NA)) %>% 
    mutate(value_cod = ifelse(is.na(value_cod2), value_cod, value_cod2)) %>% 
    select(-value_cod2) %>% 
    #Slope the cut-off
    mutate(end_start = x[min(which(is.na(value_cod) & x > max(x, na.rm=TRUE)/2))],
           end_end = x[max(which(is.na(value_cod)))],
           end_end = end_start + (1/3)*(end_end - end_start)) %>%
    mutate(slope = value_cod) %>%
    fill(slope) %>%
    mutate(slope = ifelse(!is.na(value_cod) & x < end_start, NA,
                          slope*((end_end - x) / (end_end - end_start))^(1/2))) %>%
    mutate(value_cod = ifelse(!is.na(value_cod), value_cod,
                              ifelse(slope > value_act, slope, NA))) %>%
    select(-end_start, -end_end, -slope) %>%
    #Slope the cut-off
    arrange(desc(x)) %>%
    mutate(x = max(x) - x + 1) %>%
    mutate(end_start = x[min(which(is.na(value_cod) && x > max(x, na.rm=TRUE)/2))],
           end_end = x[max(which(is.na(value_cod)))],
           end_end = end_start + (1/3)*(end_end - end_start)) %>%
    mutate(slope = value_cod) %>%
    fill(slope) %>%
    mutate(slope = ifelse(!is.na(value_cod) & x < end_start, NA,
                          slope*((end_end - x) / (end_end - end_start))^(1/2))) %>%
    mutate(value_cod = ifelse(!is.na(value_cod), value_cod,
                              ifelse(slope > value_act, slope, NA))) %>%
    select(-end_start, -end_end, -slope) %>%
    arrange(desc(x)) %>%
    mutate(x = max(x) - x + 1) %>%
    #SMooth more
    mutate(value_cod2 = (lag(value_cod, 2) + lag(value_cod) + 
                           value_cod*2 + 
                           lead(value_cod) + lead(value_cod, 2))/6) %>% 
    mutate(value_cod = ifelse(is.na(value_cod2), value_cod, value_cod2)) %>% 
    select(-value_cod2)
  
  #Raise the CoD line if it's below the minimum y of the dino for any x
  dino_silho <- dino_long %>% 
    select(-value) %>% 
    arrange(x) %>% 
    left_join(dino_rejig, by = c("x")) %>% 
    filter(!is.na(value_act)) %>% 
    mutate(first = x[min(which(!is.na(value_cod)))],
           last = x[max(which(!is.na(value_cod)))]) %>% 
    fill(value_cod) %>% 
    mutate(value_cod = ifelse(x >= first & x <= last, value_cod, value_act)) %>% 
    group_by(x) %>% 
    mutate(check = any(value_cod < min(y))) %>% 
    ungroup() %>% 
    mutate(adjust = ifelse(check, (y - value_cod)+15, NA)) %>% 
    group_by(x) %>% 
    mutate(adjust = ifelse(check, min(adjust, na.rm=T), 0)) %>% 
    ungroup() %>% 
    mutate(adjust = ifelse(any(check), max(adjust, na.rm=T), 0),
           adjust = ifelse(is.na(adjust), 0, adjust)) %>%
    rowwise() %>% 
    mutate(y_cod = ifelse(x >= first & x <= last, floor(value_cod + adjust), value_cod)) %>% 
    ungroup() %>% 
    select(x, y, y_cod, first, last, adjust) %>% 
    group_by(x) %>% 
    mutate(min_dino = min(y)) %>% 
    ungroup()
  
  dino_silho_cod <- dino_silho %>% 
    select(-y) %>% 
    distinct()
  
  
  dino_silho_comb <- expand.grid(y = as.numeric(max(c(dino_silho$y, dino_silho$y_cod))):1,
                                 x = 1:max(dino_silho$x)) %>% 
    left_join(dino_silho %>% select(x, y) %>% mutate(orig = 1), by = c("x", "y")) %>% 
    left_join(dino_silho_cod %>% select(x, y=y_cod) %>% mutate(cod = 1), by = c("x", "y")) %>% 
    mutate_all(as.numeric) %>% 
    group_by(x) %>% 
    mutate(start = cumsum(if_else(is.na(cod), 0, 1)),
           start_orig = cumsum(if_else(is.na(orig), 0, 1))) %>% 
    mutate(datasaur = cod) %>% 
    fill(datasaur) %>% 
    ungroup() %>% 
    mutate(datasaur = case_when(
      start == 1 & !is.na(orig) ~ 1,
      start == 1 & start_orig == 0 ~ 1,
      start == 0 ~ 0,
      start == 1 & is.na(orig) ~ 0,
      TRUE ~ 0
    )) %>% 
    filter(datasaur ==  1) %>% 
    select(-orig, -cod, -start) %>% 
    mutate(Line = "value_cod", Chart = "Datasaur") %>% 
    bind_rows(dino_silho %>% 
                select(x, y) %>% 
                mutate(Line = "value_act", Chart = " Original"))

  #RETURN
  dino_silho_out <- dino_silho_comb %>% 
    select(Chart, Line, x, y)
  
  out.list <- list(
    datasaur_name = dino_name,
    corrs = corrs,
    line_chart_data = dino_cor,
    naked_datasaur = dino_silho_out
  )
  
  return(out.list)
}

#Color the output from naked_datasaur()
skin_datasaur <- function(naked_datasaur, color_pattern){
 
  naked_0 <- naked_datasaur$naked_datasaur
  
  #Other Featurs
  line_chart_data <- naked_datasaur$line_chart_data
  corrs <- naked_datasaur$corrs
  datasaur_name <- naked_datasaur$datasaur_name
  
  ###
  # Get hexcolors of input color categories
  ###
  colorChoices <- read.csv("BotInputs/Colors.csv", stringsAsFactors = F)
  
  sel_color <- c()
  sel_color[1] <- colorChoices %>% 
    filter(Category == color_pattern$col1) %>% 
    select(Shade) %>% 
    sample_n(1) %>% 
    as.character()
  
  sel_color[2] <- colorChoices %>% 
    filter(Category == color_pattern$col2) %>% 
    filter(Shade != sel_color[1]) %>% 
    select(Shade) %>% 
    sample_n(1) %>% 
    as.character()
  
  ###
  # Pattern
  ###
   pattern <- color_pattern$pattern
   
   if(pattern == "spotted"){
     
     color_radius <- sample(seq(40, 125, 5), 1)
     
     wghts <- rnorm(2, 100, 20)
     wghts <- wghts / sum(wghts)
     
     skin_0 <- naked_0 %>% 
       select(Line, Chart, x, y) %>% 
       mutate(x_cat = x %/% color_radius + 1,
              y_cat = y %/% color_radius + 1) %>% 
       group_by(Line, Chart, x_cat, y_cat) %>% 
       mutate(x_rank = rank(x), y_rank = rank(y)) %>% 
       mutate(x_val = abs(x_rank - median(x_rank)),
              y_val = abs(y_rank - median(y_rank))) %>% 
       mutate(x_wght = x_val / max(x_val),
              y_wght = y_val / max(y_val)) %>% 
       mutate(weight = wghts[1] * (x_wght + y_wght),
              weight = ifelse(is.nan(weight), 1, weight)) %>% 
       ungroup()
     
     fade_y <- runif(1, min = 1, max = 2)
     
     #Select green based on weight
     skin_1 <- skin_0 %>%
       group_by(x_cat) %>%
       mutate(y_prob = (y / max(y, na.rm=T))^fade_y,
              weight = weight * y_prob) %>%
       mutate(weight = ifelse(weight > 1, 1, weight)) %>%
       ungroup() %>%
       rowwise() %>%
       mutate(color = ifelse(
         Chart == "Datasaur",  sample(sel_color, 1, prob = c(weight, 1- weight)),
         "#CCCCCC"
       )) %>%
       ungroup()
     
     #Save pattern details
     pattern_specs <- list(pattern = "spotted", 
                           radius = color_radius, weights = wghts, 
                           fade_y = fade_y)
   }
   if(pattern == "striped"){
     stripe_radius <- sample(seq(10, 50, 5), 1)
     
     stripe_direction <- runif(1, -2, 2) #Negatives slope up, <1 is flatter, >1 is steeper
     
     skin_1 <- naked_0 %>% 
       select(Line, Chart, x, y) %>% 
       mutate(stripe_cat = (x + stripe_direction*y) %/% stripe_radius + 1) %>% 
       mutate(stripe_rank = stripe_cat %% 2) %>%
       mutate(color = case_when(
         Chart == " Original" ~ "#CCCCCC",
         stripe_rank == 0 ~ sel_color[1],
         stripe_rank == 1 ~ sel_color[2],
         TRUE ~ "#CCCCCC"
       )) 
     
     #Save pattern details
     pattern_specs <- list(pattern = "striped", 
                           width = stripe_radius, direction = stripe_direction)
     
   }
   if(pattern == "diamond"){
     dot_radius <- sample(seq(10, 40, 2), 1)
     dot_radius2 <- sample(seq(10, 40, 2), 1)
     
     pow_1 <- sample(1:2, 1)
     
     sel_rank <- sample(c("sum", "mult"), 1)
     if(pow_1 == 2){ sel_rank <- "mult"} #Otherwise, we end up in Geometric territory
     
     coef_x <- sample(5:15, 1)/10
     coef_y <- sample(5:15, 1)/10
     
     skin_1 <- naked_0 %>% 
       select(Line, Chart, x, y) %>% 
       mutate(stripe_cat = (coef_x*x^pow_1 + coef_y*y^pow_1) %/% dot_radius + 1,
              stripe_cat2 = (coef_x*x - coef_y*y) %/% dot_radius2 + 1) %>% 
       do(
         if(sel_rank == "sum"){
           mutate(., stripe_rank = (stripe_cat + stripe_cat2) %% 2)
         } else {
           mutate(., stripe_rank = (stripe_cat * stripe_cat2) %% 2)
         }
       ) %>%
       mutate(color = case_when(
         Chart == " Original" ~ "#CCCCCC",
         stripe_rank == 0 ~ sel_color[1],
         stripe_rank == 1 ~ sel_color[2],
         TRUE ~ "#CCCCCC"
       )) 

     #Save pattern details
     pattern_specs <- list(pattern = "diamond", 
                           radius = c(dot_radius, dot_radius2),
                           power = pow_1, coefs = c(coef_x, coef_y),
                           operation = sel_rank)
     
   }
   if(pattern == "dotted"){
     group_radius <- sample(seq(20, 150, 5), 1)
     dot_sizes <- sample(1:5, 1)
     dot_radius <- sample(seq(5, 42), dot_sizes)/100
     
     skin_1 <- naked_0 %>% 
       select(Line, Chart, x, y) %>% 
       mutate(group_x = x %/% group_radius, 
              group_y = y %/% group_radius) %>% 
       group_by(Chart, group_x, group_y) %>% 
       mutate(p_dot  = (group_x + group_y) %% dot_sizes,
              n_dot = n(),
              x_mid = median(x, na.rm=TRUE), 
              y_mid = median(y, na.rm=TRUE)) %>% 
       mutate(p_dist = ((x-x_mid)^2 + (y-y_mid)^2)^(1/2)) %>% 
       ungroup() %>% 
       mutate(color = case_when(
         Chart == " Original" ~ "#CCCCCC",
         p_dist <  dot_radius[p_dot+1]*group_radius*n_dot/max(n_dot) ~ sel_color[2],
         p_dist >= dot_radius[p_dot+1]*group_radius*n_dot/max(n_dot) ~ sel_color[1],
         TRUE ~ "#CCCCCC"
       )) 

     #Save pattern details
     pattern_specs <- list(pattern = "dotted", 
                           group_radius = group_radius, 
                           dot_sizes = dot_sizes, dot_radius = dot_radius)
   }
   if(pattern == "3dotted"){
     group_radius <- sample(seq(20, 150, 5), 1)
     dot_sizes <- sample(1:5, 1)
     dot_radius <- sample(seq(5, 42), dot_sizes)/100
     
     #Alpha parameters
     y_weight <- sample(2:10, 1)
     radius_overflow <- 1+runif(1, 0.05, 0.30)
     x_offset <- round(runif(1, -group_radius, group_radius)/10)
     y_offset <- round(runif(1, -group_radius, group_radius)/10)
     
     skin_1 <- naked_0 %>% 
       select(Line, Chart, x, y) %>% 
       mutate(group_x = x %/% group_radius, 
              group_y = y %/% group_radius) %>% 
       group_by(Chart, group_x, group_y) %>% 
       mutate(p_dot  = (group_x + group_y) %% dot_sizes,
              n_dot = n(),
              x_mid = median(x, na.rm=TRUE), 
              y_mid = median(y, na.rm=TRUE)) %>% 
       mutate(p_dist = ((x-x_mid)^2 + (y-y_mid)^2)^(1/2)) %>% 
       ungroup() %>% 
       mutate(color = case_when(
         Chart == " Original" ~ "#CCCCCC",
         p_dist <  dot_radius[p_dot+1]*group_radius*n_dot/max(n_dot) ~ sel_color[2],
         p_dist >= dot_radius[p_dot+1]*group_radius*n_dot/max(n_dot) ~ sel_color[1],
         TRUE ~ "#CCCCCC"
       )) 

     shadowed_2 <- skin_1 %>% 
       filter(Chart == "Datasaur") %>% 
       mutate(m_n_dot = max(n_dot)) %>% 
       group_by(Chart, group_x, group_y) %>% 
       mutate(in_circle = p_dist <  (dot_radius[p_dot+1]*group_radius*n_dot/m_n_dot)*radius_overflow) %>% 
       mutate(alpha = case_when(
         in_circle ~ ((x-median(x)+x_offset)^2 + (y-median(y)+y_offset)^2)^(1/2),
         TRUE ~ 0
       )) %>% 
       ungroup() %>% 
       mutate(alpha = case_when(
         !in_circle ~ 1 - (x+y_weight*y)/(max(x)+y_weight*max(y)),
         TRUE ~ alpha^(3/2)
       )) %>% 
       group_by(in_circle) %>% 
       mutate(alpha = alpha / max(alpha, na.rm=TRUE) * 0.8) %>% 
       ungroup()
     
     #Save pattern details
     pattern_specs <- list(pattern = "3dotted", 
                           group_radius = group_radius, 
                           dot_sizes = dot_sizes, dot_radius = dot_radius, 
                           alpha_y_weight = y_weight, alpha_offset = c(x_offset, y_offset),
                           alpha_overflow = radius_overflow)
   }
   if(pattern == "geometric"){
     stripe_radius <- sample(seq(10, 100, 5), 1)
     
     stripe_direction <- runif(1, -2, 2) 
     
     power_x <- sample(c(seq(1.2, 2, by=0.1), rep(2, 5)), 1)
     power_y <- sample(c(rep(power_x, 10), seq(1.2, 2, by=0.1)), 1)
     
     skin_1 <- naked_0 %>% 
       select(Line, Chart, x, y) %>% 
       mutate(stripe_cat = (x^power_x + stripe_direction*y^power_y) %/% stripe_radius + 1) %>% 
       mutate(stripe_rank = stripe_cat %% 2) %>%
       mutate(color = case_when(
         Chart == " Original" ~ "#CCCCCC",
         stripe_rank == 0 ~ sel_color[1],
         stripe_rank == 1 ~ sel_color[2],
         TRUE ~ "#CCCCCC"
       )) 
     
     #Save pattern details
     pattern_specs <- list(pattern = "geometric", 
                           radius = stripe_radius, direction = stripe_direction,
                           power = c(power_x, power_y))
     
   }
   if(pattern == "america"){
     star_radius <- 30
     stripe_radius <- 25
     
     sel_color <- c("#4040FF", "#FFDDDD", "#FFFFFF", "#FF4040")
     
     skin_1 <- naked_0 %>% 
       select(Line, Chart, x, y) %>% 
       #Stars
       mutate(star_cat = (x+y) %/% star_radius,
              star_cat2 = (x - y) %/% star_radius) %>% 
       mutate(star_rank = (star_cat * star_cat2) %% 2) %>%
       mutate(color_star = case_when(
         star_rank == 0 ~ sel_color[1],
         star_rank == 1 ~ sel_color[3],
         TRUE ~ "#CCCCCC"
       )) %>% 
       #Stripes
       mutate(stripe_cat = y %/% stripe_radius,
              stripe_rank = stripe_cat %% 2) %>% 
       mutate(color_stripe = case_when(
         stripe_rank == 0 ~ sel_color[4],
         stripe_rank == 1 ~ sel_color[2],
         TRUE ~ "#CCCCCC"
       )) %>% 
       #Stars & Stripes
       mutate(color = case_when(
         Chart == " Original" ~ "#CCCCCC",
         x < max(x)/2.75 & y > max(y)/2 ~ color_star,
         TRUE ~ color_stripe
       ))

     #Save pattern details
     pattern_specs <- list(pattern = "america", radius = c(star_radius, stripe_radius))
     
   }
   if(pattern == "rainbow"){
     rb_band <- sample(15:20, 1)
     rb_start <- sample(seq(250,350, 5), 1)
     
     rb_colors <- c("#FF0000", "#FF8800", "#FFFF00", "#00FF00", "#0000FF", "#8800FF", "#FF00FF")
     
     rb_x_max <- max(naked_0$x, na.rm = TRUE)
     rb_x_min <- min(naked_0$x, na.rm = TRUE)
     
     rb_center_x <- floor(runif(1, min = rb_x_min, max = rb_x_max))
     rb_center_y <- 0.35 * abs(rb_center_x - rb_x_max/2) - 20 #20 knocks the center down a bit
     
     skin_1 <- naked_0 %>% 
       select(Line, Chart, x, y) %>% 
       group_by(Chart) %>% 
       mutate(p_dist = ((x - rb_center_x)^2 + (y - rb_center_y)^2)^(1/2)) %>% 
       ungroup() %>% 
       mutate(color = case_when(
         Chart == " Original" ~ "#CCCCCC",
         p_dist < rb_start ~ sel_color[1],
         p_dist < rb_start + rb_band*1 ~ rb_colors[7],
         p_dist < rb_start + rb_band*2 ~ rb_colors[6],
         p_dist < rb_start + rb_band*3 ~ rb_colors[5],
         p_dist < rb_start + rb_band*4 ~ rb_colors[4],
         p_dist < rb_start + rb_band*5 ~ rb_colors[3],
         p_dist < rb_start + rb_band*6 ~ rb_colors[2],
         p_dist < rb_start + rb_band*7 ~ rb_colors[1],
         TRUE ~ sel_color[1]
       )) 
     
     #Save pattern details
     pattern_specs <- list(pattern = "rainbow", 
                           center = c(rb_center_x, rb_center_y),
                           band = rb_band, start = rb_start,
                           colors = rb_colors)
   }
   if(pattern == "hearts"){
     
     group_radius <- c(sample(seq(50, 120, 5), 1))
     
     skin_1 <- naked_0 %>% 
       select(Line, Chart, x, y) %>% 
       mutate(group_x = x %/% group_radius, 
              group_y = y %/% group_radius) %>% 
       group_by(Chart, group_x, group_y) %>% 
       mutate(n_dot = n(), #Number of pix actually in group, for scaling later
              # Unlike the circles, consider full group size rather than present pixels
              x_mid = (group_x + 0.5)* group_radius,
              y_mid = (group_y + 0.5)* group_radius,
              x_mid_left = (group_x + 0.25)* group_radius,
              x_mid_rght = (group_x + 0.75)* group_radius
       ) %>% 
       mutate(p_dist_from_left = ((x-x_mid_left)^2 + (y-y_mid)^2)^(1/2),
              p_dist_from_rght = ((x-x_mid_rght)^2 + (y-y_mid)^2)^(1/2)) %>% 
       ungroup() %>% 
       mutate(color = case_when(
         Chart == " Original" ~ "#CCCCCC",
         #Top Left hump
         (y >= y_mid & x <= x_mid) & p_dist_from_left < (group_radius/4) ~ sel_color[2],
         #Top right hump
         (y >= y_mid & x >= x_mid) & p_dist_from_rght < (group_radius/4) ~ sel_color[2],
         #Bottom  
         (y < y_mid) & ((y_mid - y) <= abs(x - x_mid)) ~ sel_color[2],
         TRUE ~ sel_color[1]
       )) 
     
     #Save pattern details
     pattern_specs <- list(pattern = "hearts", 
                           group_radius = group_radius)
   }
   if(pattern == "feathered"){
     fthr_radius <- 5
     group_radius <- sample(seq(50, 100, 5), 1)
     
     fthr_direction <- runif(1, -2, 2) #Negatives slope up, <1 is flatter, >1 is steeper
     
     skin_1 <- naked_0 %>% 
       select(Line, Chart, x, y) %>%
       mutate(group_x = x %/% group_radius, 
              group_y = y %/% group_radius) %>% 
       #Calculate Feather Direction... compare #x to #y
       group_by(Chart, group_x, group_y, x) %>% 
       mutate(y_in_group = n()) %>% 
       ungroup() %>% 
       group_by(Chart, group_x, group_y, y) %>% 
       mutate(x_in_group = n()) %>% 
       ungroup() %>% 
       group_by(Chart, group_x, group_y) %>% 
       mutate(fthr_x = max(y_in_group, na.rm=TRUE) / max(x_in_group, na.rm=TRUE),
              fthr_y = 1 / fthr_x) %>% 
       mutate(fthr_cat = (fthr_x*x + fthr_y*(y)*2) %/% fthr_radius + 1) %>% 
       mutate(fthr_rank = fthr_cat %% 6) %>%
       ungroup() %>% 
       mutate(fthr_shift = (group_x*group_y) %% 6) %>% 
       mutate(color = case_when(
         Chart == " Original" ~ "#CCCCCC",
         fthr_rank == fthr_shift ~ sel_color[2],
         TRUE ~ sel_color[1]
       )) 
     
     #Save pattern details
     pattern_specs <- list(pattern = "feathered",
                           radius = fthr_radius, direction = fthr_direction)
     
   }
   
   ###
   # Alpha layer ----
   ###
   if(!(pattern %in% c("3dotted"))){
     sel_alpha <- sample(2:9, 1)/10
     sel_alpha_y <- sample(5:10, 1) #increase minimum to reduce jump-off points

     shadowed_2 <- skin_1 %>% 
       filter(Chart == "Datasaur") %>% 
       mutate(alpha_y = (1- y / max(y, na.rm = TRUE))) %>% 
       mutate(alpha_x = (1 - x / max(x, na.rm = TRUE))) %>%
       mutate(alpha_xy = (1 - (x+y) / max((x+y), na.rm = TRUE))) %>%
       mutate(alpha = (alpha_x  + sel_alpha_y*alpha_y )/(sel_alpha_y+2) * sel_alpha) %>% 
       group_by(y) %>%
       mutate(alpha2 = (lag(alpha, 2) + lag(alpha, 1) + alpha + lead(alpha, 1) + lead(alpha, 2))/5,
              alpha2 = ifelse(is.na(alpha2), alpha, alpha2)
       ) %>%
       ungroup() %>%
       select(-alpha) %>%
       rename(alpha = alpha2)
   }
   
   #OUTPUT
   out.list <- list(
     datasaur_name = datasaur_name,
     corrs = corrs,
     color = sel_color,
     pattern = color_pattern,
     line_chart_data = line_chart_data,
     naked_datasaur = naked_0,
     skin_datasaur = skin_1,
     skin_meta = pattern_specs,
     shadow_datasaur = shadowed_2
   )

  return(out.list)
  
}

#Plot the datasaur
plot_datasaur <- function(skin_datasaur0){
  
  #Features
  datasaur_name   <- skin_datasaur0$datasaur_name
  naked_datasaur  <- skin_datasaur0$naked_datasaur
  skin_datasaur   <- skin_datasaur0$skin_datasaur
  skin_meta       <- skin_datasaur0$skin_meta
  shadow_datasaur <- skin_datasaur0$shadow_datasaur
  line_chart_data <- skin_datasaur0$line_chart_data
  corrs           <- skin_datasaur0$corrs
  sel_color       <- skin_datasaur0$color
  
  #Details
  dino_info <- read.csv("BotInputs/DatasaurList.csv", stringsAsFactors = FALSE)
  info <- dino_info %>% 
    filter(Fauna == datasaur_name)
  
  if(!is.na(info$Twitter[1]) & info$Twitter[1] != ""){
    artist_twitter <- paste0("@", as.character(info$Twitter[1]), "")
  } else {
    artist_twitter <- NULL
  }
  
  map_cod_detail <- read.csv("CoD/map_cod_detail.csv", stringsAsFactors = FALSE)
  corrs <- corrs %>% 
    left_join(map_cod_detail, by = "Detail")
  
  #Annual X labels... 
  xlabs <- line_chart_data %>% 
    filter(Line == "value_act") %>% 
    mutate(YM = ifelse(Year %% 2 == 0 & Month == 1, paste0(Year, " M", Month), NA)) %>% 
    select(x, YM) %>% 
    filter(!is.na(YM)) %>% 
    group_by(YM) %>% 
    filter(row_number() == 1)
  
  inset_chart <- ggplot(line_chart_data, 
                        aes(x = x, y = value, group=Line, color=Line)) +
    geom_raster(data=skin_datasaur %>% filter(Chart == " Original"), 
                aes(x=x, y=y), fill = "#cccccc")+
    geom_line(size = 1.5) +
    scale_color_manual(values = c("value_cod" = "#FC3D32", "value_act" = sel_color[1])) + 
    coord_equal(expand=TRUE) +
    scale_y_continuous(limits=c(0, NA), breaks = NULL) +
    scale_x_continuous(labels = xlabs$YM, breaks = xlabs$x, name = NULL) +
    labs(caption = "Source: CDC.gov") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(color="white", size = 12),
          axis.text.x = element_text(angle = 45, hjust=1, color = "black"),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 20, face="bold.italic"),
          plot.background = element_rect(fill = "#FFFFE0", color = "#FFFFE0"),
          panel.background = element_rect(fill = "white", color = sel_color[1]),
          plot.caption = element_text(color = "#00436b", size = 10)
    )
  
  main_chart <- ggplot(line_chart_data, 
                  aes(x = x, y = value, group=Line, color=Line)) + 
    geom_raster(data=skin_datasaur %>% filter(Chart == "Datasaur"), 
                aes(x=x, y=y, fill=color))+
    scale_fill_identity() +
    geom_raster(data=shadow_datasaur %>% filter(Chart == "Datasaur"), 
                aes(x=x, y=y, alpha = alpha), fill = "#111111")+
    scale_alpha_identity() +
    scale_y_continuous(limits=c(0, NA), breaks = NULL) +
    coord_equal(expand = FALSE) +
    labs(caption = paste(datasaur_name, "by", as.character(info$Credit[1]),
                         artist_twitter)) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_blank(),
          plot.caption = element_text(size = 12)
    )
  
  ##
  #Create combined chart
  ##
  
  #If chart isn't too long, inset goes next to datasaur
    #Lowest x that doesnt have <y in it
  
  lng.x <- max(line_chart_data$x, na.rm=TRUE)
  lng.y <- max(line_chart_data$value, na.rm=TRUE)
  
  ratio.xy <- lng.x / lng.y
  
  if(lng.x < 2.1*lng.y){
    lmat <- rbind(c(2, 3), c(1, 3))
    lwdth <- c(1, 2)
    lhgt  <- c(4/3, 3)
  } else {
    #Long charts - inset on top
    lmat <- rbind(c(1, 2), c(3, 3))
    lwdth <- c(2, 1)
    lhgt  <- c(4/3, 2)
  }
  

  cod_details <- arrangeGrob(
    grobs = list(
      #Corrlations
      textGrob(paste0(round(as.numeric(corrs[1, "cor"]), 2), " "),
               gp = gpar(fontsize=30, fontface = "bold", col = "#FC3D32"),
               x = unit(0, "npc"), y = unit(.9, "npc"),
               just ="left"
               ),
      textGrob(wrapper(paste("correlation with U.S. deaths by",
                              as.character(corrs[1, "Series"]), 
                              "in", as.character(corrs[1, "Detail_print"])), 32),
                gp = gpar(fontsize=16, col = "#00436b"),
                x = unit(0, "npc"), y = unit(.7, "npc"),
                just ="left")
      ),
    nrow = 2,
    heights = c(1, 2),
    top = " ", 
    bottom = " ", 
    left = " ",
    clip = "off"
  )
  

  #Save Chart
  twit.width  <- 1024 #pixels
  twit.height <-512 #pixels
  twit.dpi <- 300
  
  plot.file <- paste0("BotRuns/v1p0 ", substr(Sys.time(), 1, 13),".png")

  png(filename = plot.file,
      bg = "#FFFFE0",
      width = (twit.width)/1.25, height = (twit.height*1.25)/1.25)
  
    grid.arrange(
        grobs = list(arrangeGrob(inset_chart), 
                     cod_details,
                     main_chart),
        widths = lwdth,
        heights = lhgt,
        clip = "on",
        layout_matrix = lmat,
        top = textGrob(paste0(datasaur_name), 
                       gp=gpar(col = "#00436b", fontsize=28, fontface = "bold"), 
                       x = 0, just = "left"),
        bottom = textGrob(paste("@Datasaurs v1.0.1"),
                       gp = gpar(fontsize = 13, fontface = "bold",
                                 col = "#00436b"),
                       x = 1, just = "right"),
        left = " ", right = " "
      )
    dev.off()#End print
  
    out.list <- c(skin_datasaur0,
                  list(
                    filename = plot.file
                  ))
  
  return(out.list)
}

#Tweet-text datasaur
text_datasaur <- function(plot_datasaur0){
  
  #Features
  datasaur_name   <- plot_datasaur0$datasaur_name
  corrs           <- plot_datasaur0$corrs
  pattern         <- plot_datasaur0$pattern
  
  #Details
  dino_info <- read.csv("BotInputs/DatasaurList.csv", stringsAsFactors = FALSE)
  info <- dino_info %>% 
    filter(Fauna == datasaur_name)
  
  map_cod_detail <- read.csv("CoD/map_cod_detail.csv", stringsAsFactors = FALSE)
  corrs <- corrs %>% 
    left_join(map_cod_detail, by = "Detail")
  
  text_cor <- paste0(round(as.numeric(corrs[1, "cor"]), 2), "")
  text_cod <- paste0("correlation with U.S. deaths by ",
                    as.character(corrs[1, "Series"]), 
                    " in ", as.character(corrs[1, "Detail_print"]), ".")
  
  #Hashtags!
  hashes_list <- c("#rstats" = 5, 
                   "#dinosaurs" = 5, "#dinos" = 2, 
                   "#dataviz" = 5, "#dataisbeautiful" = 2, "#data" = 1,
                   "#machinelearning" = 3, "#datascience" = 4, "#analytics" = 1,
                   "#science" = 2, "#statistics" = 2,
                   "#paleontology" = 2, "#paleobiology" = 2, 
                   "#bioinformatics" = 3,
                   "#themoreyouknow"= 1, "#lifefindsaway" = 1,
                   "#JurassicWorld" = 1, "#FallenKingdom" = 1,
                   "#JurassicPark" = 1, "#JurassicPark25" = 1)
  
  if(weekdays(Sys.Date()) == "Friday"){
    hashes_list <- c("#FossilFriday" = 40, #Very High Chance of #FossilFriday 
                     hashes_list)
  }
  
  if(months.Date(Sys.Date()) == "November"){
    hashes_list <- c("#Dinovember" = 25, #High chance of #Dinovember
                     hashes_list)
  }
  
  if(sample(c(TRUE, FALSE), 1, prob = c(10, 1))){
    text_hashtags <- paste0(sample(names(hashes_list), 1, prob = hashes_list),
                           #Additional hashtags
                           if(pattern$holidatasaur){" #Holidatasaur"}, 
                           if(pattern$america){" #Americasaur"},
                           if(pattern$valentinesaur){" #Valentinesaur"},
                           if(pattern$stpatrick){" #StPaddatasaur"},
                           if(pattern$pridesaur){" #PRIDEsaur"},
                           if(pattern$newyearsaur){paste(" #HappyNewYears", 
                                                         paste0(" #NY", lubridate::year(Sys.Date()+1)))} # +1 to account for NYE
    )
  } else {
    text_hashtags <- ""
  }
  
  
  #Artist tags
  if(as.character(info[1, "Twitter"]) != "" && !is.na(info[1, "Twitter"]) &&
     sample(c(TRUE, FALSE), 1, prob = c(1, 9))){
    text_twitter <- paste0("Image by @", as.character(info$Twitter[1]), ". ")
  } else {
    text_twitter <- ""
  }
  
  #Tweet text
  text_final <- paste0(
    datasaur_name, ": ", text_cor, " ", text_cod, " ",
    text_twitter, text_hashtags
  )
  
  #Output
  out.list <- c(plot_datasaur0,
                list(
                  twitter_text = text_final
                ))
  
  return(out.list)

}
