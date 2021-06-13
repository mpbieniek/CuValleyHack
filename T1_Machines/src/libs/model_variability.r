#################
### LIBRARIES ###
#################

library(xts)
library(lubridate)
library(tidyverse)

################
### SETTINGS ###
################

FALSE -> TEST_MODE

#################
### FUNCTIONS ###
#################

test_var_exc <- function(df, var_name, fst_dt, mid_dt, lst_dt) {
  df %>%
    mutate(dt = format(timestamp, '%Y-%m-%d')) %>%
    filter(dt < fst_dt) %>%
    pull(var_name) -> x_tr
  x_tr[!is.na(x_tr)] -> x_tr
  
  quantile(x_tr, probs = 0.01, na.rm = TRUE) -> x_q01
  quantile(x_tr, probs = 0.05, na.rm = TRUE) -> x_q05
  quantile(x_tr, probs = 0.10, na.rm = TRUE) -> x_q10
  quantile(x_tr, probs = 0.90, na.rm = TRUE) -> x_q90
  quantile(x_tr, probs = 0.95, na.rm = TRUE) -> x_q95
  quantile(x_tr, probs = 0.99, na.rm = TRUE) -> x_q99
  
  df %>%
    mutate(dt = format(timestamp, '%Y-%m-%d')) %>%
    filter(fst_dt <= dt, dt < mid_dt) %>%
    pull(var_name) -> x_ts_g
  x_ts_g[!is.na(x_ts_g)] -> x_ts_g
  
  df %>%
    mutate(dt = format(timestamp, '%Y-%m-%d')) %>%
    filter(mid_dt <= dt, dt <= lst_dt) %>%
    pull(var_name) -> x_ts_b
  x_ts_b[!is.na(x_ts_b)] -> x_ts_b
  
  tibble(
    name = c(
      'p_q01', 'p_q05', 'p_q10', 'p_q90', 'p_q95', 'p_q99'
    ),
    expected = c(0.01, 0.05, 0.10, 0.10, 0.05, 0.01),
    train = c(
      mean(x_tr <= x_q01), mean(x_tr <= x_q05), mean(x_tr <= x_q10),
      mean(x_tr >= x_q90), mean(x_tr >= x_q95), mean(x_tr >= x_q99)
    ),
    `test good` = c(
      mean(x_ts_g <= x_q01), mean(x_ts_g <= x_q05), mean(x_ts_g <= x_q10),
      mean(x_ts_g >= x_q90), mean(x_ts_g >= x_q95), mean(x_ts_g >= x_q99)
    ),
    `test bad` = c(
      mean(x_ts_b <= x_q01), mean(x_ts_b <= x_q05), mean(x_ts_b <= x_q10),
      mean(x_ts_b >= x_q90), mean(x_ts_b >= x_q95), mean(x_ts_b >= x_q99)
    )
  ) %>% mutate(
    `rat eb` = round(100 * (`test bad` - `expected`) / `expected`, 2),
    `rat bg` = round(100 * (`test bad` - `test good`) / `test good`, 2)
  )
}

############
### TEST ###
############

if(TEST_MODE) {
  setwd('C:/cuvalley/task_1/raw_logs/')
  
  readRDS('./LK3 050L/data.rds') -> data
  
  test_var_exc(data, 'ENGRPM', '2020-06-15', '2020-07-15', '2020-08-15')
}

