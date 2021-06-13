#################
### LIBRARIES ###
#################

library(lubridate)
library(tidyverse)

################
### SETTINGS ###
################

FALSE -> TEST_MODE

#################
### FUNCTIONS ###
#################

calc_stats <- function(df, funcs, fst_dt, lst_dt) {
  df %>%
    mutate(dt = format(timestamp, '%Y-%m-%d')) %>%
    filter(fst_dt <= dt, dt <= lst_dt) %>%
    select(-timestamp, -dt) -> df
  
  funcs %>% imap_dfr(function(func, func_name) {
    df %>%
      summarise_all(func) %>%
      mutate(name = func_name) %>%
      select(name, everything())
  })
}

############
### TEST ###
############

if(TEST_MODE) {
  setwd('C:/cuvalley/task_1/raw_logs/')
  
  readRDS('./LK3 050L/data.rds') -> data
  
  calc_stats(
    data,
    list(
      'avg' = function(x) mean(x, na.rm = TRUE),
      'std' = function(x) sd(x, na.rm = TRUE),
      'min' = function(x) min(x, na.rm = TRUE),
      'max' = function(x) max(x, na.rm = TRUE)
    ),
    '2020-01-01', '2020-03-31'
  )
}

