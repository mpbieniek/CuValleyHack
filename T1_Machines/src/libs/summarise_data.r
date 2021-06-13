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

summarise_df <- function(df) {
  tibble(
    `n. obs` = nrow(df),
    `n. vars` = ncol(df) - 1,
    `min date` = min(as.character(date(df$timestamp))),
    `max date` = max(as.character(date(df$timestamp)))
  )
}

summarise_df_vars <- function(df) {
  df %>% select(-timestamp) %>% imap_dfr(function(x, n) {
    tibble(
      `name` = n,
      
      `avg` = mean(x, na.rm = TRUE),
      `std` = sd(x, na.rm = TRUE),
      
      `min` = min(x, na.rm = TRUE),
      `q01` = quantile(x, na.rm = TRUE, probs = 0.01),
      `q05` = quantile(x, na.rm = TRUE, probs = 0.05),
      `med` = median(x, na.rm = TRUE),
      `q95` = quantile(x, na.rm = TRUE, probs = 0.95),
      `q99` = quantile(x, na.rm = TRUE, probs = 0.99),
      `max` = max(x, na.rm = TRUE),
      
      `n. NAs` = sum(is.na(x)),
      `p. NAs` = round(100 * mean(is.na(x)), 2)
    )
  })
}

############
### TEST ###
############

if(TEST_MODE) {
  setwd('C:/cuvalley/task_1/raw_logs/')
  
  readRDS('./LK3 050L/data.rds') -> data
  
  summarise_df(data)
  summarise_df_vars(data)
}

