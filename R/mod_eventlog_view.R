# Shiny Module for displaying events timeline from eventlog

library(dplyr)
library(bupaR)
library(edeaR)
library(shiny)
library(shinydashboard)
library(shinycssloaders)



############################
## Table View tab UI code ##
############################
tableViewTabUI <- function(id){
  ns <- NS(id)

  # Table View body contents
  body_table_view <- fluidRow(
    box(title = "Raw eventlog data",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        shinycssloaders::withSpinner(dataTableOutput(ns("datatable")))
    )
  )
  
  # Show events timeline tab
  tabItem(tabName = "table_view", body_table_view)
}



################################
## Table View tab server code ##
################################
tableViewTab <- function(input, output, session, eventlog, 
                         cols_to_drop = c("activity_instance_id", "lifecycle_id", "resource_id", ".order")){
  ns <- session$ns
  
  # Table View
  output$datatable <- renderDataTable({
    
    req(eventlog())
    
    # Don't show eventlog columns that are irrelevant to user
    eventlog() %>%
      as.data.frame() %>% 
      select(-any_of(cols_to_drop))
    
  })
  
  # Show events_timeline tab in menu
  menuItem(text = "Table view", tabName = "table_view", icon = icon("table"))
}
