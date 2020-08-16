## Define Shiny app

library(bupaR)
library(eventdataR)
library(processmapR)
library(processanimateR)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)

process_viewer <- function(min.time = 30, max.time = 600, default.time = 60) {
  
  ### UI code ###
  
  ## Header
  header <- dashboardHeader(title = "ProcessMiner")
  
  ## Sidebar items
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Load data", icon = icon("database"), startExpanded = TRUE,
               menuSubItem(text = "Data upload", tabName = "data_upload", icon = icon("upload"), selected = TRUE),
               menuSubItem(text = "Example datasets", tabName = "example_dataset", icon = icon("list"))
               ),
      menuItem(text = "Table view", tabName = "table_view", icon = icon("table")),
      menuItem(text = "Summary statistics", tabName = "summary_statistics", icon = icon("chart-bar")),
      menuItem(text = "Process flow", tabName = "process_flow", icon = icon("project-diagram")),
      menuItem(text = "Timeline view", tabName = "timeline_view", icon = icon("clock")),
      menuItem(text = "About ProcessMiner", tabName = "about_this_app", icon = icon("info-circle"))
    )
  )
  
  
  ## Components for dashboard body
  
  body_data_upload <- fluidRow(
    box(title = "File upload",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        fileInput(inputId = "eventlogFile", label = "Upload eventlog (.xls, .xlsx)", accept = c(".xls", ".xlsx"))
    ),
    uiOutput(outputId = "data_sample_box"),
    uiOutput(outputId = "variable_selection_box")
  )
  
  body_example_dataset <- fluidRow(
    box(title = "Example dataset",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        uiOutput("example_dataset_selector")
    )
  )

  body_table_view <- fluidRow(
    box(title = "Raw data",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        shinycssloaders::withSpinner(dataTableOutput("datatable"))
    )
  )  
  
  body_summary_statistics <- fluidRow(
    box("TODO")
  )
  
  body_process_flow <- fluidRow(
    column(width = 3,
           uiOutput("process_flow_settings_box")
    ),
    column(width = 9,
           box(title = "Process flow diagram",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               shinycssloaders::withSpinner(processanimaterOutput("process"))
           ),
           box(title = "Selected cases",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               textOutput("token_selection")
           ),
           box(title = "Selected activities",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               textOutput("activity_selection")
           )
    )
  )
  
  body_timeline_view <- fluidRow(
    box(title = "Timeline view",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        shinycssloaders::withSpinner(plotlyOutput("plotlydottedchart"))
    ),
  )
  
  body_about_this_app <- fluidRow(
    box(title = "About ProcessMiner",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        p("ProcessMiner is a simple web-based process mining tool for exploration (and potentially prediction)."),
        p("It was created by Dennis van den Berg and uses the bupaR process mining library in R. It's current status is: work in progress."),
        p("Source code: https://github.com/dljvandenberg/processminer")
    )
  )
  

  ## Dashboard body
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "data_upload", body_data_upload),
      tabItem(tabName = "example_dataset", body_example_dataset),
      tabItem(tabName = "table_view", body_table_view),
      tabItem(tabName = "summary_statistics", body_summary_statistics),
      tabItem(tabName = "process_flow", body_process_flow),
      tabItem(tabName = "timeline_view", body_timeline_view),
      tabItem(tabName = "about_this_app", body_about_this_app)
    )
  )

  
  ui <- function(request) {
    dashboardPage(
      header,
      sidebar,
      body
    )
  }
  
  
  ### Server code ###
  
  server <- function(session, input, output) {
    
    options(shiny.maxRequestSize=30*1024^2)
    
    eventlog <- reactiveVal(eventlog_default)
    
    data <- reactive({
      
      req(input$eventlogFile)
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          # TODO: Remove data-specific configuration
          exceldata <- readxl::read_excel(input$eventlogFile$datapath, skip = 1) %>% 
            dplyr::rename_all(funs(make.names(.))) %>%
            select('Zaaknummer', 'Zaaktype', 'Product', 'Datum.afhandeling', 'Onderdeel') %>% 
            filter(Product == "Vergunning WaterWet") %>% 
            arrange(Zaaknummer, Datum.afhandeling, Onderdeel)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
      return(exceldata)
      
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
          selectInput(inputId = "case_id_var", label = "Select case_id column", choices = available_variables, selected = "none"),
          selectInput(inputId = "timestamp_var", label = "Select timestamp column", choices = available_variables, selected = "none"),
          selectInput(inputId = "activity_var", label = "Select activity column", choices = available_variables, selected = "none"),
          # TODO: add action to button
          actionButton(inputId = "generate_eventlog_from_upload_button", label = "Generate eventlog")
      )
    })
    
    
    
    output$example_dataset_selector <- renderUI({
      
      # Datasets available via eventdataR package (TODO: put either in configuration or generate from available functions in eventdataR package)
      available_datasets <- c("hospital", "hospital_billing", "patients", "sepsis", "traffic_fines")
      
      column(width = 12,
        selectInput(inputId = "selected_example_dataset", label = "Choose example eventlog", choices = c("", available_datasets), selected = ""),
        actionButton(inputId = "load_example_eventlog_button", label = "Load eventlog")
      )
    })
    
    
    # On button click 
    observeEvent(input$generate_eventlog_from_upload_button,{
      
      req(data())
      req(input$case_id_var %in% colnames(data()))
      req(input$timestamp_var %in% colnames(data()))
      req(input$activity_var %in% colnames(data()))

      # Set eventlog reactive value from data
      data() %>%
        simple_eventlog(
          case_id = input$case_id_var,
          activity_id = input$activity_var,
          timestamp = input$timestamp_var
        ) %>% 
        eventlog()
      print(paste0("DEBUG: eventlog ", input$selected_example_dataset, " loaded!"))
      print(paste0("DEBUG: nrow(eventlog()): ", nrow(eventlog())))
      print(paste0("DEBUG: class(eventlog()): ", paste0(class(eventlog()), collapse = ", ")))
      
    })
    
    
    # On button click 
    observeEvent(input$load_example_eventlog_button,{
      
      req(input$selected_example_dataset)
      
      if(input$selected_example_dataset != ""){
        # Set eventlog reactive value
        eventlog(get(input$selected_example_dataset, "package:eventdataR", inherits = FALSE))
        print(paste0("DEBUG: eventlog ", input$selected_example_dataset, " loaded!"))
        print(paste0("DEBUG: nrow(eventlog()): ", nrow(eventlog())))
        print(paste0("DEBUG: class(eventlog()): ", paste0(class(eventlog()), collapse = ", ")))
      }
      
    })
    
    
    output$datatable <- renderDataTable({
      
      req(eventlog())
      
      # Don't show eventlog columns that are irrelevant to user
      print("DEBUG: datatable from eventlog")
      print(paste0("DEBUG: nrow(eventlog()): ", nrow(eventlog())))
      irrelevant_cols <- c("activity_instance_id", "lifecycle_id", "resource_id", ".order")
      eventlog() %>%
        as.data.frame() %>% 
        select(-any_of(irrelevant_cols))
      
    })
    
    
    output$process_flow_settings_box <- renderUI({
    
      box(title = "Settings",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          selectInput(inputId = "mapType", label = "Map type", choices = c("cases", "durations"), selected = "cases"),
          selectInput(inputId = "timelineMode", label = "Timeline mode", choices = c("relative", "absolute"), selected = "relative"),
          selectInput(inputId = "sizeAttribute", label = "Size attribute", choices = c("none", colnames(eventlog())), selected = "none"),
          selectInput(inputId = "colorAttribute", label = "Color attribute", choices = c("none", colnames(eventlog())), selected = "none"),
          selectInput(inputId = "orientation", label = "Orientation", choices = c("horizontal"="LR", "vertical"="TB"), selected = "horizontal"),
          sliderInput(inputId = "duration", label = "Animation duration", min = min.time, max = max.time, value = default.time)
      )
      
    })
    
    
    output$token_selection <- renderText({
      
      paste0(input$process_tokens, ",")
      
    })
    
    
    output$activity_selection <- renderText({
      
      paste0(input$process_activities, ",")
      
    })
    
    
    output$process <- renderProcessanimater(expr = {
      if(input$mapType == "durations"){
        graph <- processmapR::process_map(eventlog(), render = FALSE, type = performance())
      } else {
        graph <- processmapR::process_map(eventlog(), render = FALSE)
      }
      model <- DiagrammeR::add_global_graph_attrs(graph, attr = "rankdir", value = input$orientation, attr_type = "graph")
      if (input$sizeAttribute != "none" && input$colorAttribute != "none") {
        animate_process(eventlog(), model,
                        mode = input$timelineMode,
                        legend = "color",
                        mapping = token_aes(color = token_scale(input$colorAttribute, scale = "ordinal", 
                                                                range = RColorBrewer::brewer.pal(5, "YlOrBr")),
                                            size = token_scale(input$sizeAttribute, scale = "linear", range = c(6,10))),
                        duration = input$duration,
                        token_callback_select = token_select_decoration(stroke = "red"))
      } else if (input$sizeAttribute != "none") {
        animate_process(eventlog(), model,
                        mode = input$timelineMode,
                        legend = "size",
                        mapping = token_aes(size = token_scale(input$sizeAttribute, scale = "linear", range = c(6,10))),
                        duration = input$duration,
                        token_callback_select = token_select_decoration(stroke = "red"))
      } else if (input$colorAttribute != "none") {
        animate_process(eventlog(), model,
                        mode = input$timelineMode,
                        legend = "color",
                        mapping = token_aes(color = token_scale(input$colorAttribute, scale = "ordinal", range = RColorBrewer::brewer.pal(5, "YlOrBr"))),
                        duration = input$duration,
                        token_callback_select = token_select_decoration(stroke = "red"))
      } else {
        animate_process(eventlog(), model,
                        mode = input$timelineMode,
                        duration = input$duration,
                        token_callback_select = token_select_decoration(stroke = "red"))
      }
      
    })
    
    output$plotlydottedchart <- renderPlotly(expr = {
      processmapR::plotly_dotted_chart(eventlog())
    })
    
  }
  
  
  ### Start app ###
  
  shinyApp(ui, server, options = list(height = 500))
  
}