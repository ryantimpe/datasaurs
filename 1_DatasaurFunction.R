#####
# Dino8
# Dino Charts
#####

library(png);library(tidyverse);
library(lubridate)
library(scales)
library(zoo)

cod_all <- readRDS("BotInputs/CauseOfDeath.RDS")
colorChoices <- read.csv("BotInputs/Colors.csv", stringsAsFactors = F)

dino_info <- read.csv("BotInputs/DatasaurList.csv", stringsAsFactors = F)

#Text wrapping function
wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}

#Color conversion function
convert_color_values <- function(hex_color){
  red <-  as.numeric(as.hexmode(substr(hex_color, 2, 3)))/255
  grn <-  as.numeric(as.hexmode(substr(hex_color, 4, 5)))/255
  blu <-  as.numeric(as.hexmode(substr(hex_color, 6, 7)))/255
  
  return(c(red, grn, blu))
}
merge_colors <- function(c1, c2, w1 = 0.5){
  red <- as.hexmode(round((w1*c1[1] + (1-w1)*c2[1])*255))
  if(nchar(red) == 1){ red <- paste0("0", red)}
  grn <- as.hexmode(round((w1*c1[2] + (1-w1)*c2[2])*255))
  if(nchar(grn) == 1){ grn <- paste0("0", grn)}
  blu <- as.hexmode(round((w1*c1[3] + (1-w1)*c2[3])*255))
  if(nchar(blu) == 1){ blu <- paste0("0", blu)}
  
  return(paste0("#", red, grn, blu))
}

###
# Datasaur Function
###

datasaur <- function(dino_name, col1 = "Green", col2 = "Green", pattern = "spotted"){
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
  
  
  ###
  #New Color options
  ###
  sel_color <- c()
  sel_color[1] <- colorChoices %>% 
    filter(Category == col1) %>% 
    select(Shade) %>% 
    sample_n(1) %>% 
    as.character()
  
  sel_color[2] <- colorChoices %>% 
    filter(Category == col2) %>% 
    filter(Shade != sel_color[1]) %>% 
    select(Shade) %>% 
    sample_n(1) %>% 
    as.character()
  
  #Randomize color order
  #sel_color <- sample(sel_color, 2)
  # sel_color_values <- lapply(sel_color, convert_color_values)
  
  ##
  # Spotted datasaur
  ##
  if(pattern == "spotted"){
    
    color_radius <- sample(seq(40, 125, 5), 1)
    
    wghts <- rnorm(2, 100, 20)
    wghts <- wghts / sum(wghts)
    
    dino_silho4 <- dino_silho3 %>% 
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
    
    fade_y <- runif(1, min = 0, max = 3)
    
    #Select green based on weight
    dino_silho5 <- dino_silho4 %>%
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
    
    # #Average colors for a dfade rather than picking one
    # dino_silho5 <- dino_silho4 %>% 
    #   group_by(x_cat) %>% 
    #   mutate(y_prob = (y / max(y, na.rm=T))^fade_y,
    #          weight = weight * y_prob) %>% 
    #   mutate(weight = ifelse(weight > 1, 1, weight)) %>% 
    #   ungroup() %>% 
    #   rowwise() %>%
    #   mutate(color = ifelse(
    #     Chart == "Datasaur",  merge_colors(sel_color_values[[1]], sel_color_values[[2]], w1 = weight),
    #     "#CCCCCC"
    #   )) %>% 
    #   ungroup()
    
    #Save pattern details
    pattern_specs <- list(pattern = "spotted", radius = color_radius, weights = wghts, fade_y =  fade_y)
  }
  if(pattern == "striped"){
    stripe_radius <- sample(seq(10, 50, 5), 1)

    stripe_direction <- runif(1, -2, 2) #Negatives slope up, <1 is flatter, >1 is steeper

    dino_silho4 <- dino_silho3 %>% 
      select(Line, Chart, x, y) %>% 
      mutate(stripe_cat = (x + stripe_direction*y) %/% stripe_radius + 1) %>% 
      mutate(stripe_rank = stripe_cat %% 2) %>%
      mutate(color = case_when(
        Chart == " Original" ~ "#CCCCCC",
        stripe_rank == 0 ~ sel_color[1],
        stripe_rank == 1 ~ sel_color[2],
        TRUE ~ "#CCCCCC"
      )) 
    
    dino_silho5 <- dino_silho4
    
    #Save pattern details
    pattern_specs <- list(pattern = "striped", radius = stripe_radius, direction = stripe_direction)
    
  }
  if(pattern == "diamond"){
    dot_radius <- sample(seq(10, 40, 2), 1)
    dot_radius2 <- sample(seq(10, 40, 2), 1)
    
    pow_1 <- sample(1:2, 1)
    
    sel_rank <- sample(c("sum", "mult"), 1)
    if(pow_1 == 2){ sel_rank <- "mult"} #Otherwise, we end up in Geometric territory
    
    coef_x <- sample(5:15, 1)/10
    coef_y <- sample(5:15, 1)/10
    
    dino_silho4 <- dino_silho3 %>% 
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
    
    dino_silho5 <- dino_silho4
    
    #Save pattern details
    pattern_specs <- list(pattern = "diamond", radius = c(dot_radius, dot_radius2),
                          power = pow_1, coefs = c(coef_x, coef_y),
                          operation = sel_rank)
    
  }
  if(pattern == "dotted"){
    group_radius <- sample(seq(50, 150, 5), 1)
    dot_sizes <- sample(1:5, 1)
    dot_radius <- sample(seq(10, 42), dot_sizes)/100

    dino_silho4 <- dino_silho3 %>% 
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
    
    dino_silho5 <- dino_silho4
    
    #Save pattern details
    pattern_specs <- list(pattern = "dotted", group_radius = group_radius, 
                          dot_radius = dot_radius)
  }
  if(pattern == "geometric"){
    stripe_radius <- sample(seq(10, 100, 5), 1)
    
    stripe_direction <- runif(1, -2, 2) 
    
    power_x <- sample(c(seq(1.2, 2, by=0.1), rep(2, 5)), 1)
    power_y <- sample(c(rep(power_x, 10), seq(1.2, 2, by=0.1)), 1)
    
    # power_x <- 2
    # power_y <- 2
    
    dino_silho4 <- dino_silho3 %>% 
      select(Line, Chart, x, y) %>% 
      mutate(stripe_cat = (x^power_x + stripe_direction*y^power_y) %/% stripe_radius + 1) %>% 
      mutate(stripe_rank = stripe_cat %% 2) %>%
      mutate(color = case_when(
        Chart == " Original" ~ "#CCCCCC",
        stripe_rank == 0 ~ sel_color[1],
        stripe_rank == 1 ~ sel_color[2],
        TRUE ~ "#CCCCCC"
      )) 
    
    dino_silho5 <- dino_silho4
    
    #Save pattern details
    pattern_specs <- list(pattern = "geometric", radius = stripe_radius, direction = stripe_direction,
                          power_x = power_x, power_y = power_y)
    
  }
  if(pattern == "america"){
    star_radius <- 30
    stripe_radius <- 25
    
    sel_color <- c("#4040FF", "#FFDDDD", "#FFFFFF", "#FF4040")
    
    dino_silho4 <- dino_silho3 %>% 
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
    
    dino_silho5 <- dino_silho4
    
    #Save pattern details
    pattern_specs <- list(pattern = "america", radius = c(star_radius, stripe_radius))
    
  }

  ###
  # Alpha layer ----
  ###
  sel_alpha <- sample(2:9, 1)/10
  sel_alpha_y <- sample(3:8, 1)
  
  sel_alpha_radius <- sample(seq(20, 100, 5), 1)
    
  dino_alpha <- dino_silho5 %>% 
    filter(Chart == "Datasaur") %>% 
    group_by(x %/% sel_alpha_radius) %>% 
    mutate(alpha_y = (1- y / max(y, na.rm = TRUE))) %>% 
    ungroup() %>% 
    # group_by(y %/% sel_alpha_radius) %>%
    mutate(alpha_x = (1 - x / max(x, na.rm = TRUE))) %>%
    # ungroup() %>%
    group_by(x+y %/% sel_alpha_radius) %>%
    mutate(alpha_xy = (1 - (x+y) / max((x+y), na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(alpha = (alpha_x  + sel_alpha_y*alpha_y )/(sel_alpha_y+2) * sel_alpha) %>% 
    group_by(y) %>%
    mutate(alpha2 = (lag(alpha, 2) + lag(alpha, 1) + alpha + lead(alpha, 1) + lead(alpha, 2))/5,
           alpha2 = ifelse(is.na(alpha2), alpha, alpha2)
    ) %>%
    ungroup() %>%
    select(-alpha) %>%
    rename(alpha = alpha2)
  
  #Place holder for additional edits
  dino_cor2 <- dino_cor 
  
  
  ###
  #PLOT!
  ###

  #Annual X labels... 
  #TODO: clean this
  xlabs <- dino_cor %>% 
    filter(Line == "value_act") %>% 
    mutate(YM = ifelse(Year == lag(Year), NA, paste0(Year, " M", Month))) %>% 
    select(x, YM) %>% 
    filter(!is.na(YM))
  
  
  chart <- ggplot(dino_cor2, aes(x = x, y = value, group=Line, color=Line)) + 
    geom_raster(data=dino_silho5, aes(x=x, y=y, fill=color))+
    geom_line(size = 1.5) +
    scale_color_manual(values = c("value_cod" = "#FC3D32", "value_act" = sel_color[1])) +
    scale_fill_identity() +
    geom_raster(data=dino_alpha, aes(x=x, y=y, alpha = alpha), fill = "#111111")+
    scale_alpha_identity() + 
    coord_equal() +
    facet_grid(Chart ~.) +
    scale_y_continuous(limits=c(0, NA), breaks = NULL, 
                       name = paste0("US Cause of Death:", "\n", 
                                     wrapper(as.character(corrs[1, "Series"]), 60), "\n",
                                    " (", as.character(corrs[1, "Detail"]), ")")) +
    scale_x_continuous(labels = xlabs$YM, breaks = xlabs$x, name = NULL) +
    labs(title = paste0(dino_name),
         caption = paste(dino_name, "by", as.character(info$Credit[1]), 
                         "| Cause of death data from CDC.gov", "\n", "@Datasaurs v0.2.4")) +
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
  datasaur.list[["color"]] <- sel_color
  datasaur.list[["pattern"]] <- pattern_specs
  
  return(datasaur.list)
  
}

