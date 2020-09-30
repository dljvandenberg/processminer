## Define Shiny app

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
           box(title = "Selected case",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               dataTableOutput("process_flow_selected_case")
           )#,
           # box(title = "Selected activities",
           #     status = "primary",
           #     solidHeader = TRUE,
           #     width = 12,
           #     textOutput("activity_selection")
           # )
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
      
      # TODO: Read csv files
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
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
          #menuItem(text = "Summary statistics", tabName = "summary_statistics", icon = icon("chart-bar")),   # TODO: add summary stats tab
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
    
    
    output$process_flow_settings_box <- renderUI({
      
      req(eventlog())
      
      timestamp_var <- bupaR::timestamp(eventlog())
      timestamp_min <- min(pull(eventlog(), timestamp_var))
      timestamp_max <- max(pull(eventlog(), timestamp_var))
      
      # List of size/color attributes to choose from
      cols <- colnames(eventlog())
      numeric_cols <- colnames(select(as.data.frame(eventlog()), is.numeric))
      cols_to_exclude <- c(".order", case_id(eventlog()), activity_id(eventlog()))
      color_attribute_choices <- cols[!cols %in% cols_to_exclude]
      size_attribute_choices <- numeric_cols[!numeric_cols %in% cols_to_exclude]
    
      box(title = "Settings",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          selectInput(inputId = "mapType", label = "Map type", choices = c("cases", "durations"), selected = "cases"),
          selectInput(inputId = "timelineMode", label = "Timeline mode", choices = c("relative", "absolute"), selected = "relative"),
          selectInput(inputId = "colorAttribute", label = "Color by", choices = c("<none>", color_attribute_choices), selected = "<none>"),
          selectInput(inputId = "sizeAttribute", label = "Size by", choices = c("<none>", size_attribute_choices), selected = "<none>"),
          #dateRangeInput(inputId = "dateRange", label = "Date range", min = timestamp_min, max = timestamp_max),
          #sliderInput(inputId = "timeRange", label = "Time range", min = timestamp_min, max = timestamp_max, value = c(timestamp_min, timestamp_max)),
          sliderInput(inputId = "traceFrequency", label = "Filter trace frequency (%)", min = 10, max = 100, step = 5, value = 100)
      )
      
    })
    
    
    output$process_flow_selected_case <- renderDataTable({
      
      req(input$process_tokens)
      req(length(input$process_tokens) >= 1)
      req(eventlog())
      
      # Filter on selected case_id, drop .order column
      case_id_var <- sym(case_id(eventlog()))
      timestamp_var <- sym(timestamp(eventlog()))
      eventlog() %>% 
        filter(!!case_id_var %in% input$process_tokens) %>% 
        as.data.frame() %>% 
        select(-all_of(c(".order"))) %>% 
        arrange(!!timestamp_var)
    })
    
    
    # Activity details not shown at the moment
    # output$activity_selection <- renderText({
    #   
    #   req(input$process_activities)
    #   
    #   paste0(input$process_activities, ",")
    #   
    # })
    
    
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
      
      # Default process map settings
      map_type <- frequency("absolute")
      size_mapping <- token_scale()
      color_mapping <- token_scale()
      legend_type <- NULL
      
      # Set map type
      if(input$mapType == "durations"){
        map_type <- performance(units = "days")
      }
      
      # Set size mapping
      if (input$sizeAttribute != "<none>" & input$sizeAttribute %in% colnames(plotdata)) {
        size_mapping <- token_scale(input$sizeAttribute, scale = "linear", range = c(2,10))
        legend_type <- "size"
      }
      
      # Set color mapping
      if (input$colorAttribute != "<none>" & input$colorAttribute %in% colnames(plotdata)) {
        
        # If ordinal, count unique classes
        n_unique_colorvalues <- length(unique(pull(plotdata, input$colorAttribute)))
        
        if (input$colorAttribute == timestamp(plotdata)) {
          print("INFO: colorAttribute is timestamp variable.")
          
          # Add time bins manually
          n_bins <- 5
          min_datetime <- min(plotdata[[input$colorAttribute]])
          max_datetime <- max(plotdata[[input$colorAttribute]])
          datetime_bins <- min_datetime + (seq(from = 0, to = n_bins) / n_bins) * (max_datetime - min_datetime)
          case_ids <- unique(plotdata[[case_id(plotdata)]])
          # Create dataframe with case, time, value columns (to feed into token_scale for custom coloring)
          df_datetime_bins <- data.frame(time = datetime_bins) %>% 
            arrange(time) %>% 
            mutate(value = paste0(as.character(lubridate::date(time)), " to ", as.character(lubridate::date(lead(time))))) %>% 
            head(n_bins) %>% 
            crossing(data.frame(case = case_ids))
          # Use datetime_bins for color mapping
          color_mapping <- token_scale(df_datetime_bins, scale = "ordinal", range = RColorBrewer::brewer.pal(n_bins, "YlOrBr"))
          
        } else if (n_unique_colorvalues <= 11) {
          # Use Spectral palette for ordinal scale up to 11 classes
          color_mapping <- token_scale(input$colorAttribute, scale = "ordinal", range = rev(RColorBrewer::brewer.pal(n_unique_colorvalues, "Spectral")))
          
        } else {
          # Use Spectral palette with 5 quantized bins otherwise
          color_mapping <- token_scale(input$colorAttribute, scale = "quantize", range = rev(RColorBrewer::brewer.pal(5, "Spectral")))
        }
        
        legend_type <- "color"
      }
      
      # Animated process map 
      animate_process(eventlog = plotdata, 
                      processmap = processmapR::process_map(plotdata, width = 600, height = 600, render = FALSE, type = map_type),
                      mode = input$timelineMode,
                      legend = legend_type,
                      mapping = token_aes(color = color_mapping, size = size_mapping),
                      token_callback_select = token_select_decoration(stroke = "red"))
    })
    
    output$plotlydottedchart <- renderPlotly(expr = {
      
      req(eventlog())
      
      processmapR::plotly_dotted_chart(eventlog())
    })
    
  }
  
  
  ### Start app ###
  
  shinyApp(ui, server, options = list(height = 500))
  
}