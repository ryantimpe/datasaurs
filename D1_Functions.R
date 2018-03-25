library(png);library(tidyverse);
library(lubridate)
library(scales)
library(zoo)


#Text wrapping function
wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}


#Produce Dataframe of datasaur raster points
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
  corrs_ss <- sample(1:4, 1, prob = (1/(1:4))^(2))
  
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
           end_end = end_start + (2/3)*(end_end - end_start)) %>% 
    mutate(slope = value_cod) %>% 
    fill(slope) %>% 
    mutate(slope = ifelse(!is.na(value_cod) & x < end_start, NA, 
                          slope - 100*((x - end_start) / (end_end - end_start))^(1/2))) %>% 
    mutate(value_cod = case_when(
      !is.na(value_cod) ~ value_cod,
      slope > value_act ~ slope,
      TRUE ~ 0
    )) %>% 
    mutate(value_cod = ifelse(value_cod == 0, NA, value_cod)) %>% 
    select(-end_start, -end_end, -slope) %>% 
    #Slope the cut-off
    arrange(desc(x)) %>% 
    mutate(x = max(x) - x + 1) %>% 
    mutate(end_start = x[min(which(is.na(value_cod) & x > max(x, na.rm=TRUE)/2))],
           end_end = x[max(which(is.na(value_cod)))],
           end_end = end_start + (2/3)*(end_end - end_start)) %>% 
    mutate(slope = value_cod) %>% 
    fill(slope) %>% 
    mutate(slope = ifelse(!is.na(value_cod) & x < end_start, NA, 
                          slope - 100*((x - end_start) / (end_end - end_start))^(1/2))) %>% 
    mutate(value_cod = case_when(
      !is.na(value_cod) ~ value_cod,
      slope > value_act ~ slope,
      TRUE ~ 0
    )) %>% 
    mutate(value_cod = ifelse(value_cod == 0, NA, value_cod)) %>% 
    select(-end_start, -end_end, -slope) %>% 
    arrange(x) %>% 
    mutate(x = max(x) - x + 1) 
  
  #Raise the CoD line if it's below the minimum y of the dino for any x
  dino_silho <- dino_long %>% 
    select(-value) %>% 
    arrange(x) %>% 
    left_join(dino_rejig, by = c("x")) %>% 
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
    mutate(Body_seg_check = ifelse(Body_seg_max >= 15 | Body_seg_max== max(Body_seg_max, na.rm=T), Body_seg_max, 0)) %>% 
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
  
  #RETURN
  dino_silho_out <- dino_silho3 %>% 
    select(Chart, Line, x, y)
  
  out.list <- list(
    corrs = corrs,
    data = dino_silho_out
  )
  
  return(out.list)
}