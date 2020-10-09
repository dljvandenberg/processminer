# Shiny Module for loading eventlog, either by data_upload or from example_dataset

# TODO: remove obsolete imports
library(dplyr)
library(tidyr)
library(lubridate)
library(bupaR)
library(eventdataR)
library(edeaR)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyhelper)



####################################
## Overrule global server options ##
####################################

# Set maxRequestSize for data upload of larger files
options(shiny.maxRequestSize=30*1024^2)



######################
## Helper functions ##
######################

# Add 'time_since_start' column to eventlog
add_time_since_case_start <- function(eventlog, units = "days") {
  # Determine timestamp variable from eventlog
  timestamp_var <- sym(bupaR::timestamp(eventlog))
  
  # Add time_since_start per event, since case start
  eventlog_extended <- eventlog %>% 
    bupaR::group_by_case() %>% 
    mutate(time_since_start = difftime(!!timestamp_var, min(!!timestamp_var, na.rm = TRUE), units = units)) %>% 
    bupaR::ungroup_eventlog()
  
  return(eventlog_extended)
}



###############################
## Load Eventlog tab UI code ##
###############################
loadEventLogTabUI <- function(id, type = 'data_upload'){
  ns <- NS(id)
  
  # Show different contents based on type ('data_upload' or 'example_dataset')
  if(type == 'data_upload') {
    
    # Data upload tab
    tabItem(tabName = "data_upload",
            fluidRow(
              box(title = "File upload",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fileInput(inputId = ns("eventlogFile"), label = "Upload eventlog (.xls, .xlsx)", accept = c(".xls", ".xlsx"))
              ),
              uiOutput(outputId = ns("data_sample_box")),
              uiOutput(outputId = ns("variable_selection_box"))
            )
    )
    
  } else if (type == 'example_dataset') {
    
    # Example dataset tab
    tabItem(tabName = "example_dataset",
            fluidRow(
              box(title = "Example dataset",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput(ns("example_dataset_selector"))
              )
            )
    )
  }
}



###################################
## Load Eventlog tab server code ##
###################################
loadEventLogTab <- function(input, output, session, myeventlog, type = 'data_upload', selected = FALSE){
  ns <- session$ns
  
  #####################
  ## Reactive values ##
  #####################
  
  # Raw data from data upload
  rawdata <- reactive({
    
    req(input$eventlogFile)
    
    tryCatch(
      {
        exceldata <- readxl::read_excel(input$eventlogFile$datapath) %>% 
          dplyr::rename_all(funs(make.names(.)))
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(exceldata)
    
  })
  
  
  ########################
  ## Render UI contents ##
  ########################
  
  output$data_sample_box <- renderUI({
    
    req(rawdata())
    
    output$datatable_head <- renderDataTable({
      rawdata() %>% 
        head(3)
    })
    
    box(title = "Data sample",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        dataTableOutput(outputId = ns("datatable_head"))
    )      
    
  })
  
  
  output$variable_selection_box <- renderUI({
    
    req(rawdata())
    
    available_variables <- c("", colnames(rawdata()))
    
    box(title = "Select column names",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        # Mark relevant fields in uploaded data (case_id, timestamp, activity, additional features)
        selectInput(inputId = ns("case_id_var"), label = "Select case_id column", choices = available_variables, selected = "<none>"),
        selectInput(inputId = ns("timestamp_var"), label = "Select timestamp column", choices = available_variables, selected = "<none>"),
        selectInput(inputId = ns("activity_var"), label = "Select activity column", choices = available_variables, selected = "<none>"),
        actionButton(inputId = ns("generate_eventlog_from_upload_button"), label = "Generate eventlog")
    )
  })
  
  
  output$example_dataset_selector <- renderUI({
    
    # Datasets available via eventdataR package
    available_datasets <- c("patients", "hospital_billing", "sepsis", "traffic_fines")
    
    column(width = 12,
           selectInput(inputId = ns("selected_example_dataset"), label = "Choose example eventlog", choices = c("", available_datasets), selected = "patients"),
           actionButton(inputId = ns("load_example_eventlog_button"), label = "Load eventlog")
    )
  })
  
  
  
  #####################
  ## Event observers ##
  #####################
  
  # On button click, generate eventlog from data upload
  observeEvent(input$generate_eventlog_from_upload_button,{
    
    print("DEBUG: generate_eventlog_from_upload_button event!")
    
    req(rawdata())
    req(input$case_id_var %in% colnames(rawdata()))
    req(input$timestamp_var %in% colnames(rawdata()))
    req(input$activity_var %in% colnames(rawdata()))
    
    # Create eventlog from data and save to 'myeventlog' reactive value
    rawdata() %>%
      simple_eventlog(
        case_id = input$case_id_var,
        activity_id = input$activity_var,
        timestamp = input$timestamp_var
      ) %>% 
      myeventlog()
    
    # Add time_since_start column to eventlog and set reactive value
    myeventlog() %>% 
      add_time_since_case_start() %>% 
      myeventlog()
    
    print(paste0("INFO: eventlog generated from data upload (containing ", nrow(myeventlog()), " lines)"))
    
  })
  
  
  # On button click, load selected example eventlog
  observeEvent(input$load_example_eventlog_button,{
    
    print("INFO: load_example_eventlog_button event!")
    
    req(input$selected_example_dataset)
    
    if(input$selected_example_dataset != ""){
      
      # TODO_CURRENT: troubleshoot, find out how to pass myeventlog reactiveVal to server part of module
      
      # Set eventlog reactive value
      myeventlog(get(input$selected_example_dataset, "package:eventdataR", inherits = FALSE))
      
      # Add time_since_start column to eventlog and set reactive value
      myeventlog() %>% 
        add_time_since_case_start() %>% 
        myeventlog()
      
      print(paste0("INFO: '", input$selected_example_dataset, "' eventlog loaded from package eventdataR (containing ", nrow(myeventlog()), " lines)"))
    }
    
  })
  
  
  
  #################
  ## UI contents ##
  #################
  
  # Show events_timeline tab in menu
  if(type == 'data_upload') {
    
    # Show data_upload tab in menu
    menuSubItem(text = "Data upload", tabName = "data_upload", icon = icon("upload"), selected = selected)
    
  } else if (type == 'example_dataset') {
    
    # Show example_dataset tab in menu
    menuSubItem(text = "Example datasets", tabName = "example_dataset", icon = icon("list"), selected = selected)
    
  }
  
}
