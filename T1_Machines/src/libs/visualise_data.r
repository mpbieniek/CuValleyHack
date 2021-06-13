#################
### LIBRARIES ###
#################

library(tidyverse)

################
### SETTINGS ###
################

FALSE -> TEST_MODE

#################
### FUNCTIONS ###
#################

plot_ts <- function(df, tm, val, fst_dt, mid_dt, lst_dt) {
  df %>%
    mutate(dt = format(timestamp, '%Y-%m-%d')) %>%
    filter(fst_dt <= dt, dt <= lst_dt) %>%
    mutate(grp = if_else(dt < mid_dt, 'good', 'bad')) -> df
  
  ggplot(df, aes_string(x = tm, y = val, col = 'grp')) +
    geom_line()
}

plot_ts_ccf <- function(df, tm, val1, val2, fst_dt, mid_dt, lst_dt) {
  df %>%
    mutate(dt = format(timestamp, '%Y-%m-%d')) %>%
    filter(fst_dt <= dt, dt <= lst_dt) %>%
    mutate(grp = if_else(dt < mid_dt, 'good', 'bad')) %>%
    arrange(timestamp) -> df
  
  ccf(df[, val1], df[, val2], lag.max = 20, plot = TRUE, main = '')
}

plot_ts_scatter <- function(df, tm, val1, val2, fst_dt, mid_dt, lst_dt, l) {
  df %>%
    mutate(dt = format(timestamp, '%Y-%m-%d')) %>%
    filter(fst_dt <= dt, dt <= lst_dt) %>%
    mutate(grp = if_else(dt < mid_dt, 'good', 'bad')) %>%
    arrange(timestamp) -> df
  lag(df[, val2], l) -> df[, val2]
  
  ggplot(df, aes_string(x = val1, y = val2, col = 'grp')) +
    geom_point(alpha = 0.7, size = 2)
}

plot_ts_distr <- function(df, tm, val, fst_dt, mid_dt, lst_dt) {
  df %>%
    mutate(dt = format(timestamp, '%Y-%m-%d')) %>%
    filter(fst_dt <= dt, dt <= lst_dt) %>%
    mutate(grp = if_else(dt < mid_dt, 'good', 'bad')) -> df
  
  ggplot(df, aes_string(x = val, fill = 'grp')) +
    geom_density(alpha = 0.7)
}

plot_ts_boxplot <- function(df, tm, val, fst_dt, mid_dt, lst_dt) {
  df %>%
    mutate(dt = format(timestamp, '%Y-%m-%d')) %>%
    filter(fst_dt <= dt, dt <= lst_dt) %>%
    mutate(grp = if_else(dt < mid_dt, 'good', 'bad')) -> df
  
  ggplot(df, aes_string(y = val, fill = 'grp')) +
    geom_boxplot(alpha = 0.7)
}

############
### TEST ###
############

if(TEST_MODE) {
  setwd('C:/cuvalley/task_1/raw_logs/')
  
  readRDS('./LK3 050L/data.rds') -> data
  
  plot_ts(data, 'timestamp', 'ENGRPM')
  plot_ts_distr(
    data, 'timestamp', 'ENGRPM',
    '2020-02-01', '2020-04-01', '2020-06-01'
  )
  plot_ts_boxplot(
    data, 'timestamp', 'ENGRPM',
    '2020-02-01', '2020-04-01', '2020-06-01'
  )
}

