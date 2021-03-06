# Shiny Module for visualizing process flow from eventlog

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
library(plotly)
library(RColorBrewer)



##############################
## Process Flow tab UI code ##
##############################
processFlowTabUI <- function(id){
  ns <- NS(id)
  
  # Helper texts
  process_flow_diagram_help_text <- "
    <p>This Process Flow diagram visualizes how cases have occurred over time. Specifically, it shows the <b>transitions between activities</b> including some statistics such as number of cases or throughput times (depending on input settings).</p>
    <p>Each animated circle represents a single case. When clicked on, its details will show up in the area below the diagram.</p>"
  
  # Process flow body contents
  body_process_flow <- fluidRow(
    column(width = 9,
           box(id = "process_box",
               title = "Process flow diagram",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               closable = FALSE,
               shinycssloaders::withSpinner(processanimaterOutput(ns("process"), height = 500)) %>% 
                                              shinyhelper::helper(type = "inline", title = "Process flow", content = process_flow_diagram_help_text, size = 'l')
           ),
           box(title = "Selected case",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               dataTableOutput(ns("process_flow_selected_case"))
           )
    ),
    column(width = 3,
           uiOutput(ns("process_flow_settings_box"))
    )
  )
  
  # Show process flow tab
  tabItem(tabName = "process_flow", body_process_flow)
}



##################################
## Process Flow tab server code ##
##################################
processFlowTab <- function(input, output, session, eventlog){
  ns <- session$ns
  
  # Helper texts
  process_flow_settings_help_text <- "
    <p>You can configure the following settings on the fly:
    <li><b>Map type</b>: Choose whether to display number of <i>cases</i> or average <i>durations</i> in each step of the process flow.</li>
    <li><b>Animation mode</b>: Choose whether to animate the flow of events in <i>relative time</i> (showing their time since start) or <i>absolute time</i> (i.e. showing them in the order they happened).</li>
    <li><b>Color variable</b>: Variable in the dataset for coloring the event tokens.</li>
    <li><b>Size variable</b>: Variable in the dataset for sizing the event tokens.</li>
    <li><b>Time range</b>: Only selects events within this time range.</li>
    <li><b>Minimum path frequency</b>: Only select process flow paths that occur <i>at least this many times</i> in the dataset. Filters out rare paths that may 'pollute' the diagram.</li>
    </p>"

  
  # Box with input settings for process flow diagram
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
        selectInput(inputId = ns("mapType"), label = "Map type", choices = c("cases", "durations"), selected = "cases") %>% 
          shinyhelper::helper(type = "inline", title = "Process flow settings", content = process_flow_settings_help_text, size = 'l'),
        selectInput(inputId = ns("animationMode"), label = "Animation mode", choices = c("relative time", "absolute time"), selected = "relative time"),
        selectInput(inputId = ns("colorAttribute"), label = "Color variable", choices = c("<none>", color_attribute_choices), selected = "<none>"),
        selectInput(inputId = ns("sizeAttribute"), label = "Size variable", choices = c("<none>", size_attribute_choices), selected = "<none>"),
        #dateRangeInput(inputId = ns("dateRange"), label = "Date range", min = timestamp_min, max = timestamp_max),
        sliderInput(inputId = ns("timeRange"), label = "Time range", min = timestamp_min, max = timestamp_max, value = c(timestamp_min, timestamp_max)),
        sliderInput(inputId = ns("minTraceFrequency"), label = "Minimum path frequency", min = 1, max = 100, step = 1, value = 1)
    )
    
  })
  
  
  # Data table of eventlog filtered on selected case (ie. case that user has clicked on in process flow diagram)
  output$process_flow_selected_case <- renderDataTable({
    
    req(eventlog())
    req(input$process_tokens)
    req(length(input$process_tokens) >= 1)
    
    # Filter on selected case_id, drop .order column
    case_id_var <- sym(case_id(eventlog()))
    timestamp_var <- sym(timestamp(eventlog()))
    eventlog() %>% 
      filter(!!case_id_var %in% input$process_tokens) %>% 
      as.data.frame() %>% 
      select(-all_of(c(".order"))) %>% 
      arrange(!!timestamp_var)
  }, options = list(searching = FALSE, paging = FALSE))
  
  
  # Animated process flow diagram
  output$process <- renderProcessanimater(expr = {
    
    req(eventlog())
    req(input$timeRange)
    req(input$minTraceFrequency)
    req(input$mapType)
    req(input$sizeAttribute)
    req(input$colorAttribute)
    req(input$animationMode)
    
    # Filter base eventlog
    plotdata <- eventlog() %>% 
      edeaR::filter_time_period(input$timeRange) %>% 
      edeaR::filter_trace_frequency(interval = c(input$minTraceFrequency, NA))
    
    # Default process map settings
    map_type <- frequency("absolute")
    size_mapping <- token_scale()
    color_mapping <- token_scale()
    legend_type <- NULL
    
    # Set map type
    if(input$mapType == "durations"){
      map_type <- performance(units = "days")
    }
    
    # Set animation mode (relative vs absolute time)
    if(input$animationMode == "relative time"){
      mode <- "relative"
    } else {
      mode <- "absolute" 
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
                    mode = mode,
                    legend = legend_type,
                    mapping = token_aes(color = color_mapping, size = size_mapping),
                    token_callback_select = token_select_decoration(stroke = "red"))
  })
  
  
  # Show process_flow tab in menu
  menuItem(text = "Process flow", tabName = "process_flow", icon = icon("project-diagram"))
  
}
