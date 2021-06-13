#################
### LIBRARIES ###
#################

library(tidyverse)

################
### SETTINGS ###
################

setwd('C:/cuvalley/task_1/')

####################
### ZIP UNPACKED ###
####################

list.files('./zip/', pattern = 'zip$', recursive = TRUE) %>%
  walk(function(file_name) {
    paste0('./zip/', file_name) -> input_file
    paste0('./data/', str_sub(file_name, 1, -5)) -> output_dir
    unzip(input_file, exdir = output_dir)
  })
