#################
### LIBRARIES ###
#################

library(tidyverse)

################
### SETTINGS ###
################

setwd('C:/cuvalley/task_1/raw_logs/')

############################
### EXPLARATORY ANALYSIS ###
############################

### NA VALUES ###

list.files(
  '.', pattern = 'data\\.rds',
  full.names = TRUE, recursive = TRUE
) %>% map_dfr(~readRDS(.) %>% select(-timestamp) %>% map_dbl(~mean(is.na(.))))

### VALUE RANGES ###

# ...