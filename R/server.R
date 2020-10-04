# Server logic of ProcessMiner Shiny web application

# TODO: remove obsolete imports
library(dplyr)
library(tidyr)
library(lubridate)
library(bupaR)
library(eventdataR)
library(edeaR)
library(processmapR)
library(processanimateR)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyhelper)
library(plotly)
library(RColorBrewer)



### Helper functions

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



### Server logic

server <- function(session, input, output) {
    
    options(shiny.maxRequestSize=30*1024^2)
    
    eventlog <- reactiveVal()
    
    
    data <- reactive({
        
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
    
    
    output$sidebarmenu <- renderMenu({
        
        # NULL eventlog corresponds to to data loaded. In case of no data loaded, show less menu items
        data_loaded <- !is.null(eventlog())
        
        # Define these menuitems once, in order to use them in the conditional part below
        menuitem_dataload <- menuItem(text = "Load data", icon = icon("database"), startExpanded = TRUE,
                                      menuSubItem(text = "Example datasets", tabName = "example_dataset", icon = icon("list"), selected = TRUE),
                                      menuSubItem(text = "Data upload", tabName = "data_upload", icon = icon("upload"))
        )
        menuitem_about <- menuItem(text = "About ProcessMiner", tabName = "about_this_app", icon = icon("info-circle"))
        
        if(data_loaded){
            # Show extended menu when data has been loaded
            sidebarMenu(
                menuitem_dataload,
                menuItem(text = "Table view", tabName = "table_view", icon = icon("table")),
                callModule(eventlogSummary, "summary_stats_1", myeventlog = reactive(eventlog())),
                callModule(processFlow, "process_flow_1", eventlog = reactive(eventlog())),
                menuItem(text = "Timeline view", tabName = "timeline_view", icon = icon("clock")),
                menuitem_about
            )    
            
        } else {
            # Show short menu when data has not been loaded
            sidebarMenu(
                menuitem_dataload,
                menuitem_about
            )    
            
        }
        
    })
    
    
    output$data_sample_box <- renderUI({
        
        req(data())
        
        output$datatable_head <- renderDataTable({
            data() %>% 
                head(3)
        })
        
        box(title = "Data sample",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput(outputId = "datatable_head")
        )      
        
    })
    
    
    output$variable_selection_box <- renderUI({
        
        req(data())
        
        available_variables <- c("", colnames(data()))
        
        box(title = "Select column names",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            # Mark relevant fields in uploaded data (case_id, timestamp, activity, additional features)
            selectInput(inputId = "case_id_var", label = "Select case_id column", choices = available_variables, selected = "<none>"),
            selectInput(inputId = "timestamp_var", label = "Select timestamp column", choices = available_variables, selected = "<none>"),
            selectInput(inputId = "activity_var", label = "Select activity column", choices = available_variables, selected = "<none>"),
            # selectInput(inputId = "activity_var", label = "Select activity column", choices = available_variables, selected = "<none>"),
            actionButton(inputId = "generate_eventlog_from_upload_button", label = "Generate eventlog")
        )
    })
    
    
    
    output$example_dataset_selector <- renderUI({
        
        # Datasets available via eventdataR package
        available_datasets <- c("patients", "hospital_billing", "sepsis", "traffic_fines")
        
        column(width = 12,
               selectInput(inputId = "selected_example_dataset", label = "Choose example eventlog", choices = c("", available_datasets), selected = "patients"),
               actionButton(inputId = "load_example_eventlog_button", label = "Load eventlog")
        )
    })
    
    
    # On button click 
    observeEvent(input$generate_eventlog_from_upload_button,{
        
        req(data())
        req(input$case_id_var %in% colnames(data()))
        req(input$timestamp_var %in% colnames(data()))
        req(input$activity_var %in% colnames(data()))
        
        # Create eventlog from data and save to 'eventlog' reactive value
        data() %>%
            simple_eventlog(
                case_id = input$case_id_var,
                activity_id = input$activity_var,
                timestamp = input$timestamp_var
            ) %>% 
            eventlog()
        
        # Add time_since_start column to eventlog and set reactive value
        eventlog() %>% 
            add_time_since_case_start() %>% 
            eventlog()
        
        print(paste0("INFO: eventlog generated from data upload (containing ", nrow(eventlog()), " lines)"))
        
    })
    
    
    # On button click 
    observeEvent(input$load_example_eventlog_button,{
        
        req(input$selected_example_dataset)
        
        if(input$selected_example_dataset != ""){
            # Set eventlog reactive value
            eventlog(get(input$selected_example_dataset, "package:eventdataR", inherits = FALSE))
            
            # Add time_since_start column to eventlog and set reactive value
            eventlog() %>% 
                add_time_since_case_start() %>% 
                eventlog()
            
            print(paste0("INFO: '", input$selected_example_dataset, "' eventlog loaded from package eventdataR (containing ", nrow(eventlog()), " lines)"))
        }
        
    })
    
    
    output$datatable <- renderDataTable({
        
        req(eventlog())
        
        # Don't show eventlog columns that are irrelevant to user
        irrelevant_cols <- c("activity_instance_id", "lifecycle_id", "resource_id", ".order")
        eventlog() %>%
            as.data.frame() %>% 
            select(-any_of(irrelevant_cols))
        
    })
    
    
    output$plotlydottedchart <- renderPlotly(expr = {
        
        req(eventlog())
        
        processmapR::plotly_dotted_chart(eventlog())
    })
    
}