#################
### LIBRARIES ###
#################

library(tictoc)
library(lubridate)
library(tidyverse)
library(furrr)

###############
### SOURCES ###
###############

setwd('C:/Users/pawel/Dysk Google/hackathony/cuvalley/rozwi¹zania/')
setwd('./ANALIZA DANYCH MASZYN SMG/code/')
source('./libs/load_data.r')

################
### SETTINGS ###
################

setwd('C:/cuvalley/task_1/raw_logs/')

plan(multisession(workers = 4))

#########################
### CHECK CORRECTNESS ###
#########################

### TWO SETS OF LOGS INSIDE ONE FILE ###

list.dirs('.', recursive = FALSE) %>% walk(function(dir1) {
  list.dirs(dir1, recursive = FALSE) %>% walk(function(dir2) {
    list.dirs(dir2, recursive = FALSE) %>% walk(function(dir3) {
      cat('[', dir3, ']\n', sep = '')
      list.dirs(dir3, recursive = FALSE) %>% future_walk(function(dir4) {
        list.files(dir4, pattern = 'csv$') %>% walk(function(file) {
          if(sum(readLines(file.path(dir4, file)) == 'ASCII') > 1) {
            print(file.path(dir4, file))
          }
        })
      })
    })
  })
})

### ALMOST EMPTY FILES ###

list.dirs('.', recursive = FALSE) %>% walk(function(dir1) {
  list.dirs(dir1, recursive = FALSE) %>% walk(function(dir2) {
    list.dirs(dir2, recursive = FALSE) %>% walk(function(dir3) {
      cat('[', dir3, ']\n', sep = '')
      list.dirs(dir3, recursive = FALSE) %>% future_walk(function(dir4) {
        list.files(dir4, pattern = 'csv$') %>% walk(function(file) {
          if(file.size(file.path(dir4, file)) <= 320) {
            print(file.path(dir4, file))
          }
        })
      })
    })
  })
})

### CSV READING TEST ###

list.dirs('.', recursive = FALSE) %>% walk(function(dir1) {
  list.dirs(dir1, recursive = FALSE) %>% walk(function(dir2) {
    list.dirs(dir2, recursive = FALSE) %>% walk(function(dir3) {
      list.dirs(dir3, recursive = FALSE) %>% future_walk(function(dir4) {
        list.files(dir4, pattern = 'csv$') %>% walk(function(file) {
          cat('[', file, ']\n', sep = '')
          read.table(file.path(dir4, file), skip = 4, sep = '|')
        })
      })
    })
  })
})

##################
### CLEAN DATA ###
##################

list.dirs('.', recursive = FALSE) %>% walk(function(dir1) {
  list.dirs(dir1, recursive = FALSE) %>% walk(function(dir2) {
    list.dirs(dir2, recursive = FALSE) %>% walk(function(dir3) {
      cat('[', dir3, ']\n', sep = '')
      list.dirs(dir3, recursive = FALSE) %>% future_walk(function(dir4) {
        list.files(dir4, pattern = 'csv$') %>% walk(function(file) {
          if(file.size(file.path(dir4, file)) == 0) {
            print(file.path(dir4, file))
            file.remove(file.path(dir4, file))
          }
        })
      })
    })
  })
})

########################
### LOGS AGGREGATION ###
########################

### MONTHS ###

list.dirs('.', recursive = FALSE) %>% walk(function(dir1) {
  list.dirs(dir1, recursive = FALSE) %>% walk(function(dir2) {
    cat('[', dir2, ']\n', sep = '')
    list.dirs(dir2, recursive = FALSE) %>% future_walk(function(dir3) {
      read_log_data_from_dir(dir3) -> logs
      saveRDS(logs, file = paste0(dir3, '/logs.rds'))
    })
  })
})

### PATH 1
'./LK3 045L' %>% walk(function(dir1) {
  list.dirs(dir1, recursive = FALSE) %>% walk(function(dir2) {
    cat('[', dir2, ']\n', sep = '')
    list.dirs(dir2, recursive = FALSE) %>% future_walk(function(dir3) {
      read_log_data_from_dir(
        dir3,
        function(log_path, log_type = 'type 2', trace = FALSE) {
          read_log_data(log_path, log_type, trace)
        }
      ) -> logs
      saveRDS(logs, file = paste0(dir3, '/logs.rds'))
    })
  })
})

### SIZE REDUCTION ###

list.files('.', pattern = 'logs\\.rds', recursive = TRUE, full.names = TRUE) %>%
  future_walk(function(log_path) {
    readRDS(log_path) -> logs
    logs %>%
      filter(!is.na(value), info_type == 'D') %>%
      select(
        -department, -info_type, -smg, -machine_type, -machine_number,
        -location, -data_type, -unit, -op_type, -values_in, -quality
      ) %>%
      distinct() %>%
      saveRDS(paste0(str_sub(log_path, 1, -5), '_v2.rds'))
  }, .progress = TRUE)

### MACHINES (WITH AGGREGATION) ###

list.dirs('.', recursive = FALSE) %>% walk(function(dir1) {
  cat('[', dir1, ']\n', sep = '')
  list.files(
    dir1, pattern = 'logs_v2\\.rds',
    recursive = TRUE, full.names = TRUE
  ) %>%
    future_map_dfr(function(logs_name) {
      readRDS(logs_name) -> logs
      0 -> second(logs$timestamp)
      logs %>%
        group_by(var_name, timestamp) %>%
        summarise(value = mean(value), .groups = 'drop')
    }, .progress = TRUE) %>%
    saveRDS(file = file.path(dir1, 'logs_v3.rds'))
})

#######################
### PREPARE SAMPLES ###
#######################

### CHECK CONTENT ###

for(file_name in list.dirs('.', recursive = FALSE)) {
  cat(' --- ', file.path(file_name, 'logs_v3.rds'), ' --- \n', sep = '')
  print(readRDS(file.path(file_name, 'logs_v3.rds')))
}

### CHECK NAs ###

for(file_name in list.dirs('.', recursive = FALSE)) {
  cat(' --- ', file.path(file_name, 'logs_v3.rds'), ' --- \n', sep = '')
  print(sum(is.na(readRDS(file.path(file_name, 'logs_v3.rds'))$value)))
}

### TRANSFORM LOGS TO DATA ###

list.files(
  '.', pattern = 'logs_v3\\.rds',
  recursive = TRUE, full.names = TRUE
) %>% walk(function(logs_path) {
  cat('[', logs_path, ']\n', sep = '')
  readRDS(logs_path) %>%
    convert_logs_to_data() %>%
    saveRDS(paste0(str_sub(logs_path, 1, -12), 'data.rds'))
})
