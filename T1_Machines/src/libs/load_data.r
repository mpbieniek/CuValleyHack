#################
### LIBRARIES ###
#################

library(tidyverse)

################
### SETTINGS ###
################

# FIRST FREE ERROR CODE: 10

FALSE -> TEST_MODE

#################
### LOAD LOGS ###
#################

### LOG TYPE 1 ###

read_log_header_type_1 <- function(log_path) {
  read.table(log_path, skip = 2, nrows = 1, sep = '|') -> header_1
  read.table(log_path, skip = 3, nrows = 1, sep = '|') -> header_2
  
  header_1 %>%
    transmute(
      ascii = V1,
      time_format = case_when(
        V2 == 0 ~ 'UTC',
        V2 == 1 ~ 'LOCAL',
        TRUE ~ 'error (id=4) (load_data::read_log_header_type_1)'
      ),
      time_zone = V3,
      na_block_beh = case_when(
        V4 == 0 ~ 'Do not create replacement blocks.',
        V4 == 1 ~ 'Create replacement blocks.',
        V4 == 10 ~ 'FastLoad ORIGINAL VALUES specified by TagName.',
        V4 == 11 ~ 'FastLoad ORIGINAL VALUES specified by TagKey.',
        TRUE ~ 'error (id=5) (load_data::read_log_header_type_1)'
      ),
      ts_rep_blocks = case_when(
        V5 == 0 ~ 'Recreate blocks from first point to present time.',
        V5 == 1 ~ 'Recreate blocks for the duration of CSV data.',
        V5 == 2 ~ 'Do not propagate the last values (for FastLoad only).',
        TRUE ~ 'error (id=6) (load_data::read_log_header_type_1)'
      )
    ) -> header_1
  
  header_2 %>% transmute(
    name = V1,
    op_type = case_when(
      V2 == 0 ~ 'ORIGINAL VALUE',
      V2 == 1 ~ 'INSERT',
      V2 == 2 ~ 'UPDATE',
      V2 == 3 ~ 'MULTIPOINT UPDATE',
      TRUE ~ 'error (id=7) (load_data::read_log_header_type_1)'
    ),
    timestamp = strptime(paste0(V3, ' ', V4), '%Y/%m/%d %H:%M:%OS'),
    values_in = case_when(
      V5 == 0 ~ 'EU',
      V5 == 1 ~ 'RAW',
      TRUE ~ 'error (id=8) (load_data::read_log_header_type_1)'
    ),
    value = V6,
    quality = V7
  ) -> header_2
  
  header_2 %>%
    separate(
      name,
      into = c(
        'department', 'info_type', 'smg',
        'machine_type', 'machine_number', 'location'
      ),
      sep = c(2, 3, 7, 12, 16, 17),
      remove = TRUE
    ) %>%
    mutate_at(
      vars(department, info_type, smg, machine_type, machine_number, location),
      ~str_remove_all(., '_')
    ) -> header_2
  
  header_1 %>%
    bind_cols(header_2) %>%
    as_tibble()
}

read_log_data_type_1 <- function(log_path) {
  read.table(log_path, skip = 4, sep = '|') %>% as_tibble() -> data
  
  data %>% transmute(
    name = V1,
    op_type = case_when(
      V2 == 0 ~ 'ORIGINAL VALUE',
      V2 == 1 ~ 'INSERT',
      V2 == 2 ~ 'UPDATE',
      V2 == 3 ~ 'MULTIPOINT UPDATE',
      TRUE ~ 'error (id=1) (load_data::read_log_data_type_1)'
    ),
    timestamp = strptime(paste0(V3, ' ', V4), '%Y/%m/%d %H:%M:%OS'),
    values_in = case_when(
      V5 == 0 ~ 'EU',
      V5 == 1 ~ 'RAW',
      TRUE ~ 'error (id=2) (load_data::read_log_data_type_1)'
    ),
    value = V6,
    quality = V7
  ) -> data
  
  data %>%
    separate(
      name,
      into = c(
        'department', 'info_type', 'smg',
        'machine_type', 'machine_number', 'location',
        'var_name', 'data_type', 'unit'
      ),
      sep = c(2, 3, 7, 13, 16, 17, 28, 29),
      remove = TRUE
    ) %>%
    mutate_at(
      vars(
        department, info_type, smg, machine_type, machine_number, location,
        var_name, data_type, unit
      ),
      ~str_remove_all(., '_')
    )
}

### LOG TYPE 2 ###

read_log_data_type_2 <- function(log_path) {
  read.table(log_path, skip = 6, sep = '|') %>% as_tibble() -> data
  
  data %>% transmute(
    name = V1,
    op_type = case_when(
      V2 == 0 ~ 'ORIGINAL VALUE',
      V2 == 1 ~ 'INSERT',
      V2 == 2 ~ 'UPDATE',
      V2 == 3 ~ 'MULTIPOINT UPDATE',
      TRUE ~ 'error (id=1) (load_data::read_log_data_type_1)'
    ),
    timestamp = strptime(paste0(V3, ' ', V4), '%Y/%m/%d %H:%M:%OS'),
    values_in = case_when(
      V5 == 0 ~ 'EU',
      V5 == 1 ~ 'RAW',
      TRUE ~ 'error (id=2) (load_data::read_log_data_type_1)'
    ),
    value = V6,
    quality = V7
  ) -> data
  
  data %>%
    separate(
      name,
      into = c(
        'department', 'info_type', 'smg',
        'machine_type', 'machine_number', 'location',
        'var_name', 'data_type', 'unit'
      ),
      sep = c(2, 3, 7, 13, 16, 17, 28, 29),
      remove = TRUE
    ) %>%
    mutate_at(
      vars(
        department, info_type, smg, machine_type, machine_number, location,
        var_name, data_type, unit
      ),
      ~str_remove_all(., '_')
    )
}

### ALL LOG TYPES ###

read_log_header <- function(log_path, log_type = 'type 1') {
  if(log_type == 'type 1') {
    read_log_header_type_1(log_path)
  } else {
    stop('error (id=9) (load_data::read_log_header)')
  }
}

read_log_data <- function(log_path, log_type = 'type 1', trace = FALSE) {
  if(trace) {
    cat('[', log_path, ']\n', sep = '')
  }
  
  if(log_type == 'type 1') {
    read_log_data_type_1(log_path)
  } else if(log_type == 'type 2') {
    read_log_data_type_2(log_path)
  }else {
    stop('error (id=3) (load_data::read_log_data)')
  }
}

### LOG STRUCTURE ###

read_log_data_from_dir <- function(
  log_dir, file_parse_func = read_log_data, trace = FALSE
) {
  list.files(log_dir, recursive = TRUE, pattern = 'csv$', full.names = TRUE) %>%
    map_dfr(file_parse_func, trace = trace)
}

####################
### CONVERT LOGS ###
####################

convert_logs_to_data <- function(logs) {
  logs %>%
    # filter(info_type == 'D', !is.na(value)) %>%
    # select(machine_type, machine_number, timestamp, var_name, value) %>%
    select(timestamp, var_name, value) %>%
    distinct() %>%
    pivot_wider(
      # id_cols = c(machine_type, machine_number, timestamp),
      id_cols = c(timestamp),
      names_from = var_name, values_from = value
    )
}

# convert_logs_to_alarms <- function(logs) {
#   logs %>%
#     distinct() %>%
#     filter(info_type == 'E', !is.na(value)) %>%
#     transmute(
#       machine_type, machine_number, timestamp,
#       var_name, value = TRUE
#     ) %>%
#     pivot_wider(
#       id_cols = c(machine_type, machine_number, timestamp),
#       names_from = var_name, values_from = value, values_fill = FALSE
#     )
# }

#############
### TESTS ###
#############

if(TEST_MODE) {
  setwd('C:/cuvalley/task_1/data/LK3 045L/')
  
  list.files('.', 'csv$') %>% map_dfr(read_log_header) %>% print()
  list.files('.', 'csv$') %>% map_dfr(read_log_data) %>% print()
  
  list.files('.', 'csv$') %>% map_dfr(read_log_data) -> logs
  logs %>% count(timestamp)
  
  convert_logs_to_data(logs)
  convert_logs_to_alarms(logs)
  
  read_log_data_from_dir('.', trace = TRUE) -> logs
}

# 1. Czy mog¹ byæ ró¿ne jednostki u¿ywane w pomiarze tej samej zmiennej?
# 2. Czy s¹ stosowane ró¿ne op_type?