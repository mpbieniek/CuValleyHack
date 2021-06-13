#################
### LIBRARIES ###
#################

library(MSGARCH)
library(lubridate)
library(tidyverse)

###############
### SOURCES ###
###############

setwd('C:/Users/pawel/Dysk Google/hackathony/cuvalley/rozwi¹zania/')
setwd('./ANALIZA DANYCH MASZYN SMG/code/libs/')

source('./feature_engineering.r')

################
### SETTINGS ###
################

FALSE -> TEST_MODE

#################
### FUNCTIONS ###
#################

'2020-01-15' -> fst_dt
'2020-06-15' -> mid_dt
'2020-12-15' -> lst_dt

data %>%
  mutate(dt = format(timestamp, '%Y-%m-%d')) %>%
  filter(fst_dt <= dt, dt <= lst_dt) %>%
  arrange(timestamp) %>%
  pull('ENGRPM') -> x
x[!is.na(x)] -> x

CreateSpec(
  variance.spec = list(model = 'sGARCH'),
  distribution.spec = list(distribution = 'sstd'),
  switch.spec = list(do.mix = FALSE, K = 2)
) -> model_spec_2
FitML(model_spec_2, x) -> model_2
summary(model_2)

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

