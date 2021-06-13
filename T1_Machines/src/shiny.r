#################
### LIBRARIES ###
#################

library(lubridate)
library(DT)
library(shiny)
library(shinythemes)

###############
### SOURCES ###
###############

setwd('C:/Users/pawel/Dysk Google/hackathony/cuvalley/rozwiązania/')
setwd('./ANALIZA DANYCH MASZYN SMG/code/libs/')

source('./model_ml.r')
source('./model_variability.r')
source('./feature_engineering.r')
source('./summarise_data.r')
source('./visualise_data.r')

################
### SETTINGS ###
################

setwd('C:/cuvalley/task_1/raw_logs/')

############
### DATA ###
############

readRDS('./LK3 045L/data.rds') -> data

##############
### CLIENT ###
##############

ui <- fluidPage(
  titlePanel('MADSztygar'),
  theme = shinytheme('paper'),
  # themeSelector(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        'machine',
        label = 'Maszyna:',
        choices = list.dirs('.', full.names = FALSE, recursive = FALSE)
      ),
      dateRangeInput(
        'timespan',
        label = 'Zakres czasowy:',
        start = as.Date('2019-01-01'), end = as.Date('2021-12-31')
      ),
      selectInput(
        'aggregation',
        label = 'Granulacja danych:',
        choices = c('minuta', 'godzina', 'dzien'),
        selected = 'dzien'
      ),
      dateInput(
        'target',
        label = 'Data zdarzenia',
        value = as.Date('2020-12-31')
      ),
      numericInput(
        'wnd_size',
        label = 'Rozmiar ruchomego okna (dni)',
        min = 5, max = 30, step = 1, value = 10
      ),
      actionButton('execute', label = 'Wykonaj')
    ),
    mainPanel(tabsetPanel(
      tabPanel(
        'Dane',
        tableOutput('df_summary'),
        tableOutput('df_vars_summary')
      ),
      tabPanel(
        'Analiza 1D',
        fluidRow(
          column(8,
            selectInput(
              'analyse_1d_var_name',
              label = 'Zmienna:',
              choices = c('')
            ),
            selectInput(
              'analyse_1d_var_type',
              label = 'Przeksztalcenie:',
              choices = c('identycznosc', 'roznica')
            ),
            fluidRow(
              column(4, dateInput('analyse_1d_fst_dt', 'Analiza od:')),
              column(4, dateInput('analyse_1d_mid_dt', 'Problem od:')),
              column(4, dateInput('analyse_1d_lst_dt', 'Awaria:'))
            ),
            plotOutput('analyse_1d_ts_plot'),
            fluidRow(
              column(6, plotOutput('analyse_1d_distr_plot')),
              column(6, plotOutput('analyse_1d_boxplot')),
            )
          ),
          column(4, tableOutput('analyse_1d_stats'))
        )
      ),
      tabPanel(
        'Analiza 2D',
        fluidRow(
          column(4, dateInput('analyse_2d_fst_dt', 'Analiza od:')),
          column(4, dateInput('analyse_2d_mid_dt', 'Problem od:')),
          column(4, dateInput('analyse_2d_lst_dt', 'Awaria:'))
        ),
        fluidRow(
          column(6,
            selectInput(
              'analyse_2d_var_name_1',
              label = 'Zmienna 1:',
              choices = c('')
            ),
            selectInput(
              'analyse_2d_var_type_1',
              label = 'Przeksztalcenie:',
              choices = c('identycznosc', 'roznica')
            ),
            plotOutput('analyse_2d_ts_plot_1')
          ),
          column(6,
            selectInput(
              'analyse_2d_var_name_2',
              label = 'Zmienna 2:',
              choices = c('')
            ),
            selectInput(
              'analyse_2d_var_type_2',
              label = 'Przeksztalcenie:',
              choices = c('identycznosc', 'roznica')
            ),
            plotOutput('analyse_2d_ts_plot_2')
          )
        ),
        plotOutput('analyse_2d_ts_acf'),
        plotOutput('analyse_2d_ts_scatter'),
        numericInput(
          'analyse_2d_dist',
          label = 'Przesunięcie czasowe',
          min = 0, max = 30, step = 1, value = 0
        )
      ),
      tabPanel(
        'Analiza ekstremów',
        fluidRow(
          column(4, dateInput('var_anal_fst_dt', 'Test od:')),
          column(4, dateInput('var_anal_mid_dt', 'Problem od:')),
          column(4, dateInput('var_anal_lst_dt', 'Awaria:'))
        ),
        tableOutput('var_anal_table')
      ),
      tabPanel(
        'Analiza istotności',
        fluidRow(
          column(4, dateInput('sign_anal_fst_dt', 'Analiza od:')),
          column(4, dateInput('sign_anal_mid_dt', 'Problem od:')),
          column(4, dateInput('sign_anal_lst_dt', 'Awaria:'))
        ),
        fluidRow(
          column(6, tableOutput('sign_anal_table')),
          column(6, plotOutput('sign_anal_plot'))
        )
      ),
      tabPanel(
        'Modelowanie predykcyjne',
        selectInput(
          'model_type',
          label = 'Model:',
          choices = c('Regresja Logistyczna', 'Drzewo Klasyfikacyjne', 'Las Losowy')
        ),
        fluidRow(
          column(4, dateInput('model_fst_dt', 'Analiza od:')),
          column(4, dateInput('model_mid_dt', 'Problem od:')),
          column(4, dateInput('model_lst_dt', 'Awaria:'))
        ),
        verbatimTextOutput('model_summary')
      )
    ))
  )
)

##############
### SERVER ###
##############

server <- function(input, output, session) {
  observeEvent(input$execute, {
    setwd('C:/cuvalley/task_1/raw_logs/')
    
    cat(' --- data loading ---\n', sep = '')
    readRDS(file.path(input$machine, 'data.rds')) %>%
      filter(
        input$timespan[1] <= date(timestamp),
        date(timestamp) <= input$timespan[2]
      ) ->> data
    
    cat(' --- data aggregation ---\n', sep = '')
    case_when(
      input$aggregation == 'minuta' ~ '%Y-%m-%d %H:%M',
      input$aggregation == 'godzina' ~ '%Y-%m-%d %H',
      input$aggregation == 'dzien' ~ '%Y-%m-%d',
    ) -> aggregation_formula
    as.POSIXct(strptime(data$timestamp, aggregation_formula)) ->> data$timestamp
    (function(x) mean(x, na.rm = TRUE)) -> aggregation_function
    data %>%
      group_by(timestamp) %>%
      summarise_all(aggregation_function) ->> data
    
    cat(' --- data printing ---\n', sep = '')
    print(data)
    
    cat(' --- data widgets ---\n', sep = '')
    output$df_summary <- renderTable({summarise_df(data)})
    output$df_vars_summary <- renderTable({summarise_df_vars(data)})
    
    cat(' --- 1d analysis widgets ---\n', sep = '')
    data %>% select(-timestamp) %>% names() -> var_names
    updateSelectInput(session, 'analyse_1d_var_name', choices = var_names)
    updateDateInput(session, 'analyse_1d_fst_dt', value = input$target - days(60))
    updateDateInput(session, 'analyse_1d_mid_dt', value = input$target - days(30))
    updateDateInput(session, 'analyse_1d_lst_dt', value = input$target)
    
    cat(' --- 2d analysis widgets ---\n', sep = '')
    data %>% select(-timestamp) %>% names() -> var_names
    updateSelectInput(session, 'analyse_2d_var_name_1', choices = var_names)
    updateSelectInput(session, 'analyse_2d_var_name_2', choices = var_names)
    updateDateInput(session, 'analyse_2d_fst_dt', value = input$target - days(60))
    updateDateInput(session, 'analyse_2d_mid_dt', value = input$target - days(30))
    updateDateInput(session, 'analyse_2d_lst_dt', value = input$target)
    
    cat(' --- var analysis widgets ---\n', sep = '')
    updateDateInput(session, 'var_anal_fst_dt', value = input$target - days(60))
    updateDateInput(session, 'var_anal_mid_dt', value = input$target - days(30))
    updateDateInput(session, 'var_anal_lst_dt', value = input$target)
    
    cat(' --- significance analysis widgets ---\n', sep = '')
    updateDateInput(session, 'sign_anal_fst_dt', value = input$target - days(60))
    updateDateInput(session, 'sign_anal_mid_dt', value = input$target - days(30))
    updateDateInput(session, 'sign_anal_lst_dt', value = input$target)
    
    cat(' --- model building widgets ---\n', sep = '')
    updateDateInput(session, 'model_fst_dt', value = input$target - days(60))
    updateDateInput(session, 'model_mid_dt', value = input$target - days(30))
    updateDateInput(session, 'model_lst_dt', value = input$target)
  })
  output$analyse_1d_ts_plot <- renderPlot({
    if(input$analyse_1d_var_type == 'roznica') {
      data[, input$analyse_1d_var_name] -
        lag(data[, input$analyse_1d_var_name]) ->
        data[, input$analyse_1d_var_name]
    }
    
    plot_ts(
      data, 'timestamp', input$analyse_1d_var_name,
      input$analyse_1d_fst_dt,
      input$analyse_1d_mid_dt,
      input$analyse_1d_lst_dt
    )
  })
  output$analyse_2d_ts_plot_1 <- renderPlot({
    if(input$analyse_2d_var_type_1 == 'roznica') {
      data[, input$analyse_2d_var_name_1] -
        lag(data[, input$analyse_2d_var_name_1]) ->
        data[, input$analyse_2d_var_name_1]
    }
    
    plot_ts(
      data, 'timestamp', input$analyse_2d_var_name_1,
      input$analyse_2d_fst_dt,
      input$analyse_2d_mid_dt,
      input$analyse_2d_lst_dt
    )
  })
  output$analyse_2d_ts_plot_2 <- renderPlot({
    if(input$analyse_2d_var_type_2 == 'roznica') {
      data[, input$analyse_2d_var_name_2] -
        lag(data[, input$analyse_2d_var_name_2]) ->
        data[, input$analyse_2d_var_name_2]
    }
    
    plot_ts(
      data, 'timestamp', input$analyse_2d_var_name_2,
      input$analyse_2d_fst_dt,
      input$analyse_2d_mid_dt,
      input$analyse_2d_lst_dt
    )
  })
  output$analyse_2d_ts_acf <- renderPlot({
    if(input$analyse_2d_var_type_1 == 'roznica') {
      data[, input$analyse_2d_var_name_1] -
        lag(data[, input$analyse_2d_var_name_1]) ->
        data[, input$analyse_2d_var_name_1]
    }
    
    if(input$analyse_2d_var_type_2 == 'roznica') {
      data[, input$analyse_2d_var_name_2] -
        lag(data[, input$analyse_2d_var_name_2]) ->
        data[, input$analyse_2d_var_name_2]
    }
    
    plot_ts_ccf(
      data, 'timestamp',
      input$analyse_2d_var_name_1,
      input$analyse_2d_var_name_2,
      input$analyse_2d_fst_dt,
      input$analyse_2d_mid_dt,
      input$analyse_2d_lst_dt
    )
  })
  output$analyse_2d_ts_scatter <- renderPlot({
    if(input$analyse_2d_var_type_1 == 'roznica') {
      data[, input$analyse_2d_var_name_1] -
        lag(data[, input$analyse_2d_var_name_1]) ->
        data[, input$analyse_2d_var_name_1]
    }
    
    if(input$analyse_2d_var_type_2 == 'roznica') {
      data[, input$analyse_2d_var_name_2] -
        lag(data[, input$analyse_2d_var_name_2]) ->
        data[, input$analyse_2d_var_name_2]
    }
    
    plot_ts_scatter(
      data, 'timestamp',
      input$analyse_2d_var_name_1,
      input$analyse_2d_var_name_2,
      input$analyse_2d_fst_dt,
      input$analyse_2d_mid_dt,
      input$analyse_2d_lst_dt,
      input$analyse_2d_dist
    )
  })
  output$analyse_1d_distr_plot <- renderPlot({
    if(input$analyse_1d_var_type == 'roznica') {
      data[, input$analyse_1d_var_name] -
        lag(data[, input$analyse_1d_var_name]) ->
        data[, input$analyse_1d_var_name]
    }
    
    plot_ts_distr(
      data, 'timestamp', input$analyse_1d_var_name,
      input$analyse_1d_fst_dt,
      input$analyse_1d_mid_dt,
      input$analyse_1d_lst_dt
    )
  })
  output$analyse_1d_boxplot <- renderPlot({
    if(input$analyse_1d_var_type == 'roznica') {
      data[, input$analyse_1d_var_name] -
        lag(data[, input$analyse_1d_var_name]) ->
        data[, input$analyse_1d_var_name]
    }
    
    plot_ts_boxplot(
      data, 'timestamp', input$analyse_1d_var_name,
      input$analyse_1d_fst_dt,
      input$analyse_1d_mid_dt,
      input$analyse_1d_lst_dt
    )
  })
  output$analyse_1d_stats <- renderTable({
    list(
      'avg' = function(x) mean(x, na.rm = TRUE),
      'std' = function(x) sd(x, na.rm = TRUE),
      'min' = function(x) min(x, na.rm = TRUE),
      'q01' = function(x) quantile(x, na.rm = TRUE, probs = 0.01),
      'q05' = function(x) quantile(x, na.rm = TRUE, probs = 0.05),
      'med' = function(x) median(x, na.rm = TRUE),
      'q95' = function(x) quantile(x, na.rm = TRUE, probs = 0.95),
      'q99' = function(x) quantile(x, na.rm = TRUE, probs = 0.99),
      'max' = function(x) max(x, na.rm = TRUE)
    ) -> stats_funcs
    
    if(input$analyse_1d_var_type == 'roznica') {
      data[, input$analyse_1d_var_name] -
        lag(data[, input$analyse_1d_var_name]) ->
        data[, input$analyse_1d_var_name]
    }
    
    data %>%
      select(timestamp, 'good' = input$analyse_1d_var_name) %>%
      calc_stats(
        stats_funcs,
        input$analyse_1d_fst_dt,
        input$analyse_1d_mid_dt
      ) -> stats_good
    
    data %>%
      select(timestamp, 'bad' = input$analyse_1d_var_name) %>%
      calc_stats(
        stats_funcs,
        input$analyse_1d_mid_dt,
        input$analyse_1d_lst_dt
      ) -> stats_bad
    
    stats_good %>%
      inner_join(stats_bad, by = 'name') %>%
      mutate(
        dif = bad - good,
        rat = round(100 * dif / good, 2)
      )
  })
  output$var_anal_table <- renderTable({
    data %>% select(-timestamp) %>% names() %>% map_dfr(function(var_name) {
      test_var_exc(
        data, var_name,
        input$var_anal_fst_dt,
        input$var_anal_mid_dt,
        input$var_anal_lst_dt
      ) %>%
        mutate(
          var = var_name,
          `rat bg` = if_else(is.infinite(`rat bg`), NA_real_, `rat bg`),
          `rat eb` = if_else(is.infinite(`rat eb`), NA_real_, `rat eb`)
        ) %>%
        rename(stat = name) %>%
        select(var, stat, everything())
    }) %>% arrange(desc(abs(`rat bg`)), desc(abs(`rat eb`)))
  })
  output$sign_anal_plot <- renderPlot({
    prepare_train_sample(
      data,
      as.Date(input$sign_anal_fst_dt),
      as.Date(input$sign_anal_mid_dt),
      as.Date(input$sign_anal_lst_dt),
      input$wnd_size
    ) -> train
    train %>% select(-date) %>% mutate(target = as.factor(target)) -> train
    train[, colSums(is.na(train)) < 1] -> train
    randomForest(target ~ ., train) -> model
    varImpPlot(model, sort = TRUE, n.var = 20, pch = 16, col = 'darkorange')
  })
  output$sign_anal_table <- renderTable({
    prepare_train_sample(
      data,
      as.Date(input$sign_anal_fst_dt),
      as.Date(input$sign_anal_mid_dt),
      as.Date(input$sign_anal_lst_dt),
      input$wnd_size
    ) -> train
    train %>% select(-date) %>% mutate(target = as.factor(target)) -> train
    train[, colSums(is.na(train)) < 1] -> train
    randomForest(target ~ ., train) -> model
    importance(model) %>% as.data.frame() %>% rownames_to_column('name') %>%
      arrange(desc(MeanDecreaseGini))
  })
  output$model_summary <- renderPrint({
    prepare_train_sample(
      data,
      as.Date(input$model_fst_dt),
      as.Date(input$model_mid_dt),
      as.Date(input$model_lst_dt),
      input$wnd_size
    ) -> train
    train %>% select(-date) %>% mutate(target = as.factor(target)) -> train
    train[, colSums(is.na(train)) < 1] -> train
    
    if(input$model_type == 'Regresja Logistyczna') {
      step(
        glm(target ~ 1, binomial(), train),
        list(
          lower = glm(target ~ 1, binomial(), train),
          upper = glm(target ~ ., binomial(), train)
        ),
        direction = 'both', steps = 10, k = log(nrow(train)), trace = FALSE
      ) -> model
      return(summary(model))
    } else if(input$model_type == 'Drzewo Klasyfikacyjne') {
      rpart(target ~ ., train) -> model
      return(summary(model))
    } else if(input$model_type == 'Las Losowy') {
      randomForest(target ~ ., train) -> model
      return(model)
    }
  })
}

#################
### RUN SHINY ###
#################

# MADSztygar = Maszyna Analityczno-Decyzyjna Sztygar

shinyApp(ui = ui, server = server)
