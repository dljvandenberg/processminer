## Define Shiny app

library(dplyr)
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

process_viewer <- function() {
  
  ### UI code ###
  
  ## Header
  header <- dashboardHeader(title = "ProcessMiner")
  
  
  ## Sidebar items
  sidebar <- dashboardSidebar(
    sidebarMenuOutput("sidebarmenu")
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
    column(width = 9,
           box(id = "process_box",
               title = "Process flow diagram",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               closable = FALSE,
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
    ),
    column(width = 3,
           uiOutput("process_flow_settings_box")
    )
  )
  
  body_timeline_view <- fluidRow(
    box(id = "timeline_box",
        title = "Timeline view",
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
        p("It was created by Dennis van den Berg and uses the bupaR process mining library in R."),
        p("Its current status is: experimental."),
        p("Source code: https://github.com/dljvandenberg/processminer")
    )
  )
  

  ## Dashboard body
  body <- dashboardBody(
    tags$head(tags$script('
      // Define function to set height of "process_box", "process", "timeline_box" and "plotlydottedchart"
      // See https://stackoverflow.com/questions/56965843/height-of-the-box-in-r-shiny
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 100;

        $("#process_box").height(boxHeight);
        $("#process").height(boxHeight - 30);
        
        $("#timeline_box").height(boxHeight);
        $("#plotlydottedchart").height(boxHeight - 30);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
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
    
    eventlog <- reactiveVal()
    
    
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
          menuItem(text = "Summary statistics", tabName = "summary_statistics", icon = icon("chart-bar")),
          menuItem(text = "Process flow", tabName = "process_flow", icon = icon("project-diagram")),
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
          selectInput(inputId = "case_id_var", label = "Select case_id column", choices = available_variables, selected = "none"),
          selectInput(inputId = "timestamp_var", label = "Select timestamp column", choices = available_variables, selected = "none"),
          selectInput(inputId = "activity_var", label = "Select activity column", choices = available_variables, selected = "none"),
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

      # Set eventlog reactive value from data
      data() %>%
        simple_eventlog(
          case_id = input$case_id_var,
          activity_id = input$activity_var,
          timestamp = input$timestamp_var
        ) %>% 
        eventlog()
      print(paste0("INFO: eventlog generated from data upload"))
      print(paste0("INFO: number of lines in eventlog: ", nrow(eventlog())))

    })
    
    
    # On button click 
    observeEvent(input$load_example_eventlog_button,{
      
      req(input$selected_example_dataset)
      
      if(input$selected_example_dataset != ""){
        # Set eventlog reactive value
        eventlog(get(input$selected_example_dataset, "package:eventdataR", inherits = FALSE))
        print(paste0("INFO: eventlog ", input$selected_example_dataset, " loaded from package eventdataR"))
        print(paste0("INFO: number of lines in eventlog: ", nrow(eventlog())))
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
    
    
    output$process_flow_settings_box <- renderUI({
      
      req(eventlog())
      
      timestamp_var <- bupaR::timestamp(eventlog())
      timestamp_min <- min(pull(eventlog(), timestamp_var))
      timestamp_max <- max(pull(eventlog(), timestamp_var))
    
      box(title = "Settings",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          selectInput(inputId = "mapType", label = "Map type", choices = c("cases", "durations"), selected = "cases"),
          selectInput(inputId = "timelineMode", label = "Timeline mode", choices = c("relative", "absolute"), selected = "relative"),
          selectInput(inputId = "sizeAttribute", label = "Size attribute", choices = c("none", colnames(eventlog())), selected = "none"),
          selectInput(inputId = "colorAttribute", label = "Color attribute", choices = c("none", colnames(eventlog())), selected = "none"),
          # TODO_CURRENT: add time filter option
          #dateRangeInput(inputId = "dateRange", label = "Date range", min = timestamp_min, max = timestamp_max),
          #sliderInput(inputId = "timeRange", label = "Time range", min = timestamp_min, max = timestamp_max, value = c(timestamp_min, timestamp_max)),
          sliderInput(inputId = "traceFrequency", label = "Filter trace frequency (%)", min = 10, max = 100, step = 5, value = 100)
      )
      
    })
    
    
    output$token_selection <- renderText({
      
      req(input$process_tokens)
      
      paste0(input$process_tokens, ",")
      
    })
    
    
    output$activity_selection <- renderText({
      
      req(input$process_activities)
      
      paste0(input$process_activities, ",")
      
    })
    
    
    output$process <- renderProcessanimater(expr = {
      
      req(eventlog())
      req(input$traceFrequency)
      req(input$mapType)
      req(input$sizeAttribute)
      req(input$colorAttribute)
      req(input$timelineMode)
      
      # Filter base eventlog
      plotdata <- eventlog() %>% 
        edeaR::filter_trace_frequency(percentage = input$traceFrequency / 100)
      
      # Base process map
      if(input$mapType == "durations"){
        graph <- processmapR::process_map(plotdata, width = 600, height = 600, render = FALSE, type = performance(units = "days"))
      } else {
        graph <- processmapR::process_map(plotdata, width = 600, height = 600, render = FALSE)
      }
      
      # Animated process map
      if (input$sizeAttribute != "none" && input$colorAttribute != "none") {
        animate_process(plotdata, graph,
                        mode = input$timelineMode,
                        legend = "color",
                        mapping = token_aes(color = token_scale(input$colorAttribute, scale = "ordinal", 
                                                                range = RColorBrewer::brewer.pal(5, "YlOrBr")),
                                            size = token_scale(input$sizeAttribute, scale = "linear", range = c(6,10))),
                        token_callback_select = token_select_decoration(stroke = "red"))
      } else if (input$sizeAttribute != "none") {
        animate_process(plotdata, graph,
                        mode = input$timelineMode,
                        legend = "size",
                        mapping = token_aes(size = token_scale(input$sizeAttribute, scale = "linear", range = c(6,10))),
                        token_callback_select = token_select_decoration(stroke = "red"))
      } else if (input$colorAttribute != "none") {
        animate_process(plotdata, graph,
                        mode = input$timelineMode,
                        legend = "color",
                        mapping = token_aes(color = token_scale(input$colorAttribute, scale = "ordinal", range = RColorBrewer::brewer.pal(5, "YlOrBr"))),
                        token_callback_select = token_select_decoration(stroke = "red"))
      } else {
        animate_process(plotdata, graph,
                        mode = input$timelineMode,
                        token_callback_select = token_select_decoration(stroke = "red"))
      }
      
    })
    
    output$plotlydottedchart <- renderPlotly(expr = {
      
      req(eventlog())
      
      processmapR::plotly_dotted_chart(eventlog())
    })
    
  }
  
  
  ### Start app ###
  
  shinyApp(ui, server, options = list(height = 500))
  
}