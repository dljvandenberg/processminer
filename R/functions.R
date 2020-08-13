## Define Shiny app

library(bupaR)
library(processmapR)
library(processanimateR)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)

process_viewer <- function(eventlog, min.time = 30, max.time = 600, default.time = 60) {
  
  header <- dashboardHeader(title = "ProcessMiner")
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Load data", tabName = "load_data", icon = icon("upload")),
      menuItem(text = "Table view", tabName = "table_view", icon = icon("table")),
      menuItem(text = "Summary statistics", tabName = "summary_statistics", icon = icon("chart-bar")),
      menuItem(text = "Process flow", tabName = "process_flow", icon = icon("project-diagram")),
      menuItem(text = "Timeline view", tabName = "timeline_view", icon = icon("clock")),
      menuItem(text = "About ProcessMiner", tabName = "about_processminer", icon = icon("info-circle"))
    )
  )
  
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "load_data",
              fluidRow(
                box(title = "File upload",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    fileInput(inputId = "eventlogFile", label = "Upload eventlog (.xls, .xlsx)", accept = c(".xls", ".xlsx"))
                ),
                uiOutput(outputId = "data_sample_box"),
                uiOutput(outputId = "variable_selection_box"),
              )
      ),
      tabItem(tabName = "table_view",
              fluidRow(
                box(title = "Raw data",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    dataTableOutput("datatable")
                )
              )
      ),
      tabItem(tabName = "summary_statistics",
              fluidRow(
                box("TODO")
              )
      ),
      tabItem(tabName = "process_flow",
              fluidRow(
                column(width = 3,
                       box(title = "Settings",
                           status = "primary",
                           solidHeader = TRUE,
                           width = 12,
                           selectInput(inputId = "mapType", label = "Map type", choices = c("cases", "durations"), selected = "cases"),
                           selectInput(inputId = "timelineMode", label = "Timeline mode", choices = c("relative", "absolute"), selected = "relative"),
                           selectInput(inputId = "sizeAttribute", label = "Size attribute", choices = c("none", colnames(eventlog)), selected = "none"),
                           selectInput(inputId = "colorAttribute", label = "Color attribute", choices = c("none", colnames(eventlog)), selected = "none"),
                           selectInput(inputId = "orientation", label = "Orientation", choices = c("horizontal"="LR", "vertical"="TB"), selected = "horizontal"),
                           sliderInput(inputId = "duration", label = "Animation duration", min = min.time, max = max.time, value = default.time)
                       )  
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
      ),
      tabItem(tabName = "timeline_view",
              fluidRow(
                box(title = "Timeline view",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    shinycssloaders::withSpinner(plotlyOutput("plotlydottedchart"))
                ),
              )
      ),
      tabItem(tabName = "about_processminer",
              fluidRow(
                box(title = "About ProcessMiner",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    p("ProcessMiner is a simple web-based process mining tool for exploration (and potentially prediction)."),
                    p("It was created by Dennis van den Berg and uses the bupaR process mining library in R. It's current status is: work in progress."),
                    p("Source code: https://github.com/dljvandenberg/processminer")
                )
              )
      )
    )
  )

  
  ui <- function(request) {
    dashboardPage(
      header,
      sidebar,
      body
    )
  }
  
  
  server <- function(session, input, output) {
    
    options(shiny.maxRequestSize=30*1024^2)
    
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
    
    eventlog <- reactive({
      
      # TODO: Remove data-specific configuration
      data() %>% 
        simple_eventlog(
          case_id = "Zaaknummer",
          activity_id = "Onderdeel",
          timestamp = "Datum.afhandeling"
          )
      
      # TODO: put this code in proper place
      # if (input$colorAttribute != "none") {
      #   attr <- rlang::sym(input$colorAttribute)
      #   val <- eventlog %>% pull(!!attr)
      #   if (!(is.character(val) || is.factor(val))) {
      #     warning("Trying to use a numeric attribute for the token color!")
      #   }
      # }
      # 
      # if (input$sizeAttribute != "none") {
      #   # This only works for numeric attributes
      #   attr <- rlang::sym(input$sizeAttribute)
      #   val <- eventlog %>% pull(!!attr)
      #   if (!is.numeric(val)) {
      #     warning("Trying to use a non-numeric attribute for the token size!")
      #   }
      # }

    })

    

    

    output$data_sample_box <- renderUI({
      
      req(data())
      
      output$datatable_head <- renderDataTable({
        data() %>% 
          head(5)
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
          selectInput(inputId = "activity_var", label = "Select activity column", choices = available_variables, selected = "none")
      )
    })
    
    
    output$datatable <- renderDataTable({
      
      req(data())
      
      return(data())
      
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
  
  shinyApp(ui, server, options = list(height = 500))
  
}