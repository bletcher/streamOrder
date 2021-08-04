library(tidyverse)

setwd('d:/Observables/streamOrder')

d <- read.csv('./dataIn/nhd_gages.csv')

# Get number of gaged streams by gnis_id and streamOrder
# Code each gnis_id as gaged or not
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

# Get proportion gaged by stream order for each region
d3 <- d2 %>%
  group_by(region, streamOrder) %>%
  summarise(propGaged = sum(gageSum01) / n())
    
  

##########################################################
# plots
plotfilteredbySO <- function(orderIn){
  return(
    ggplot(filter(d2, streamOrder == orderIn), aes(gnis_name, gageSum01)) +
      geom_col() +
      #scale_x_continuous() + 
      theme(axis.text.x = element_text(angle = 90)) +
      ggtitle(paste0('StreamOrder = ', orderIn )) +
      facet_wrap( ~region, scales = "free")
  )
}

plotfilteredbySO(10)

ggplot(d3, aes(streamOrder, propGaged)) +
  geom_col() +
  scale_x_continuous("Stream order", n.breaks = 10) +
  facet_wrap( ~region)

### why do big stream orders have small propGaged?
d2 %>%
  filter(streamOrder == 9)
d %>%
  filter(gnis_id == 723728)
  d3 %>%
    filter(streamOrder == 9)
  
  
  
  