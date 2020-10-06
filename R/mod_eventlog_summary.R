# Shiny Module for eventlog summary

library(dplyr)
library(tidyr)
library(bupaR)
library(edeaR)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)



##################################
## Eventlog summary tab UI code ##
##################################
eventlogSummaryTabUI <- function(id){
  ns <- NS(id)

  body_summary_statistics <- fluidRow(
    
    box(title = "Cases per activity",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        shinycssloaders::withSpinner(plotly::plotlyOutput(ns("stats_cases")))
    ),
    box(title = "Events per activity",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        shinycssloaders::withSpinner(plotly::plotlyOutput(ns("stats_events")))
    ),
    box(title = "Throughput times per case",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        shinycssloaders::withSpinner(plotOutput(ns("throughput_time_plot")))
    ),
    box(title = "Summary stats",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        shinycssloaders::withSpinner(tableOutput(ns("stats_table")))
    )
  )
  
  tabItem(tabName = "summary_statistics", body_summary_statistics)
}



######################################
## Eventlog summary tab server code ##
######################################
eventlogSummaryTab <- function(input, output, session, myeventlog, default_color = "skyblue2"){
  ns <- session$ns
  
  
  output$stats_cases <- plotly::renderPlotly({
    
    req(myeventlog())
    
    activity_var <- sym(bupaR::activity_id(myeventlog()))
    myeventlog() %>% 
      bupaR::group_by_activity() %>% 
      bupaR::n_cases() %>% 
      {ggplot(., aes(x = !!activity_var, y = n_cases)) +
          geom_col(fill = default_color) +
          ylab('Number of cases') +
          coord_flip()} %>% 
      ggplotly()
  })
  
  
  output$stats_events <- plotly::renderPlotly({
    
    req(myeventlog())
    
    activity_var <- sym(bupaR::activity_id(myeventlog()))
    myeventlog() %>% 
      bupaR::group_by_activity() %>% 
      bupaR::n_events() %>% 
      {ggplot(., aes(x = !!activity_var, y = n_events)) +
          geom_col(fill = default_color) +
          ylab('Number of events') +
          coord_flip()} %>% 
      ggplotly()
  })
  
  
  output$throughput_time_plot <- renderPlot({
    
    req(myeventlog())
    
    myeventlog() %>% 
      edeaR::throughput_time(level = "case") %>% 
      plot()
    
  })
  
  
  output$stats_table <- shiny::renderTable({
    
    req(myeventlog())
    
    n_cases <- bupaR::n_cases(myeventlog())
    n_events <- bupaR::n_events(myeventlog())
    n_activities <- bupaR::n_activities(myeventlog())
    
    data.frame(Statistic = c('Number of activities', 'Number of cases', 'Number of events'), Value = c(n_activities, n_cases, n_events))
  })
  
  
  menuItem(text = "Summary statistics", tabName = "summary_statistics", icon = icon("chart-bar"))
  
}
