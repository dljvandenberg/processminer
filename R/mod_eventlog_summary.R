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
  
  # Helper texts
  summary_stats_help_text <- "
  <p>We use the following definitions:
  <li>A <b>case</b> has a unique case id that can be tracked over time. Generally, multiple events are logged under a single case id.</li>
  <li>An <b>event</b> occurs at a given timestamp.</li>
  <li>An <b>activity</b> is the type of event. It can (but doesn't need to) have a start and end time).</li></p>"

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
        shinycssloaders::withSpinner(tableOutput(ns("stats_table"))) %>% 
          shinyhelper::helper(type = "inline", title = "Summary statistics", content = summary_stats_help_text, size = 'l')
    )
  )
  
  tabItem(tabName = "summary_statistics", body_summary_statistics)
}



######################################
## Eventlog summary tab server code ##
######################################
eventlogSummaryTab <- function(input, output, session, eventlog, default_color = "skyblue2"){
  ns <- session$ns
  
  output$stats_cases <- plotly::renderPlotly({
    
    req(eventlog())
    
    activity_var <- sym(bupaR::activity_id(eventlog()))
    eventlog() %>% 
      bupaR::group_by_activity() %>% 
      bupaR::n_cases() %>% 
      {ggplot(., aes(x = !!activity_var, y = n_cases)) +
          geom_col(fill = default_color) +
          ylab('Number of cases') +
          coord_flip()} %>% 
      ggplotly()
  })
  
  
  output$stats_events <- plotly::renderPlotly({
    
    req(eventlog())
    
    activity_var <- sym(bupaR::activity_id(eventlog()))
    eventlog() %>% 
      bupaR::group_by_activity() %>% 
      bupaR::n_events() %>% 
      {ggplot(., aes(x = !!activity_var, y = n_events)) +
          geom_col(fill = default_color) +
          ylab('Number of events') +
          coord_flip()} %>% 
      ggplotly()
  })
  
  
  output$throughput_time_plot <- renderPlot({
    
    req(eventlog())
    
    eventlog() %>% 
      edeaR::throughput_time(level = "case") %>% 
      plot()
    
  })
  
  
  output$stats_table <- shiny::renderTable({
    
    req(eventlog())
    
    n_cases <- bupaR::n_cases(eventlog())
    n_events <- bupaR::n_events(eventlog())
    n_activities <- bupaR::n_activities(eventlog())
    
    data.frame(Statistic = c('Number of activities', 'Number of cases', 'Number of events'), Value = c(n_activities, n_cases, n_events))
  })
  
  
  menuItem(text = "Summary statistics", tabName = "summary_statistics", icon = icon("chart-bar"))
  
}
