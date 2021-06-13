#################
### LIBRARIES ###
#################

library(rpart)
library(randomForest)
library(xts)
library(lubridate)
library(tidyverse)
library(furrr)

###############
### SOURCES ###
###############

setwd('C:/Users/pawel/Dysk Google/hackathony/cuvalley/rozwi¹zania/')
setwd('./ANALIZA DANYCH MASZYN SMG/code/libs/')

source('./feature_engineering.r')

################
### SETTINGS ###
################

plan(multisession)

FALSE -> TEST_MODE

list(
  'avg' = function(x) mean(x, na.rm = TRUE),
  'std' = function(x) sd(x, na.rm = TRUE),
  # 'min' = function(x) min(x, na.rm = TRUE),
  'q01' = function(x) quantile(x, na.rm = TRUE, probs = 0.01),
  'q05' = function(x) quantile(x, na.rm = TRUE, probs = 0.05),
  'med' = function(x) median(x, na.rm = TRUE),
  'q95' = function(x) quantile(x, na.rm = TRUE, probs = 0.95),
  'q99' = function(x) quantile(x, na.rm = TRUE, probs = 0.99)
  # 'max' = function(x) max(x, na.rm = TRUE)
) -> STATS_FUNCS

#################
### FUNCTIONS ###
#################

prepare_train_sample <- function(df, fst_dt, mid_dt, lst_dt, wnd_size) {
  seq(fst_dt, lst_dt - wnd_size, 'day') %>% future_map_dfr(function(wnd_dt) {
    calc_stats(
      df, STATS_FUNCS,
      fst_dt = wnd_dt, lst_dt = wnd_dt + days(wnd_size)
    ) %>%
      mutate(date = wnd_dt) %>%
      select(date, name, everything())
  }, .progress = TRUE) -> train
  
  train %>%
    pivot_longer(
      cols = c(-date, -name),
      names_to = 'var_name', values_to = 'val'
    ) %>%
    transmute(date, var_name = paste0(var_name, '_', name), val) %>%
    pivot_wider(
      id_cols = date,
      names_from = var_name, values_from = val
    ) -> train
  train %>% mutate(target = date - mid_dt + floor(wnd_size / 2) >= 0)
}

############
### TEST ###
############

if(TEST_MODE) {
  setwd('C:/cuvalley/task_1/raw_logs/')
  
  readRDS('./LK3 050L/data.rds') -> data

  prepare_train_sample(
    data,
    as.Date('2020-11-01'), as.Date('2020-12-01'), as.Date('2020-12-31'), 10
  ) -> train
  train %>% select(-date) -> train
  train %>% mutate(target = as.factor(target)) -> train
  
  step(
    glm(target ~ 1, binomial(), train),
    list(
      lower = glm(target ~ 1, binomial(), train),
      upper = glm(target ~ ., binomial(), train)
    ),
    direction = 'both', steps = 10, k = log(nrow(train))
  )
  
  rpart(target ~ ., train)
  
  randomForest(target ~ ., train) -> model
  importance(model) %>% as.data.frame() %>% rownames_to_column('name')
  varImpPlot(model)
  varImpPlot(model, sort = TRUE, n.var = 20, pch = 16, col = 'darkorange')
}
