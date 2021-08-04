library(tidyverse)

setwd('d:/Observables/streamOrder')

d <- read.csv('nhd_gages.csv')

#group <- function(groupID){
               
  d2 <- d %>%
    rename(streamOrder = streamorde,
           region = CASC) %>%
    mutate(gotGage = !is.na(site_no),
           gotGage01 = gotGage * 1) %>%
    group_by(region, gnis_id, gnis_name, streamOrder) %>%
    summarize(n = n(),
              gageSum = sum(gotGage01)) %>%
    mutate(gageSum01 = ifelse(gageSum > 0, 1, 0)) %>%
    filter(streamOrder %in% 1:10) %>%
    drop_na(gnis_id, gnis_name) %>%
    ungroup()
  
  ### why do big stream orders have small propGaged?
  d2 %>%
    filter(streamOrder == 9)
  d %>%
    filter(gnis_id == 723728)
  
  
  d3 <- d2 %>%
    group_by(region, streamOrder) %>%
    summarise(propGaged = sum(gageSum01) / n())
    
  
  d3 %>%
    filter(streamOrder == 9)
#  return(d3)
#}

#dGrouped = group(gnis_name)

d3 %>% 
  ggplot(aes(streamOrder, propGaged)) +
    geom_col() +
    scale_x_continuous("Stream order", n.breaks = 10) +
    facet_wrap(~ region)
