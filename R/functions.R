## Define Shiny app

library(bupaR)
library(processmapR)
library(processanimateR)
library(shiny)
library(shinycssloaders)

process_viewer <- function(eventlog, min.time = 30, max.time = 600, default.time = 60) {
  
  ui <- function(request) {
    fluidPage(
      tags$head(tags$style("#process{height:90vh !important;}")),
      titlePanel(title = "ProcessMiner 0.1", 
                 windowTitle = "ProcessMiner 0.1"),
      h5(tags$em("A project by Dennis van den Berg")),
      
      sidebarLayout(
        
        sidebarPanel(
          width = 2,
          fileInput("eventlogFile", "Upload eventlog (Excel)", accept = c(".xls", ".xlsx")),
          selectInput("mapType", "Map type", c("cases", "durations"), "cases"),
          selectInput("timelineMode", "Timeline mode", c("relative", "absolute"), "relative"),
          selectInput("sizeAttribute", "Size attribute", c("none", colnames(eventlog)), "none"),
          selectInput("colorAttribute", "Color attribute", c("none", colnames(eventlog)), "none"),
          selectInput("orientation", "Orientation", c("horizontal"="LR", "vertical"="TB"), "horizontal"),
          sliderInput("duration", "Animation duration", min.time, max.time, default.time),
          h4("Selected cases"),
          textOutput("token_selection"),
          h4("Selected activities"),
          textOutput("activity_selection")
        ),
        
        mainPanel(
          width = 10,
          tabsetPanel(
            tabPanel("Data", dataTableOutput("datatable")),
            tabPanel("Process flow", shinycssloaders::withSpinner(processanimaterOutput("process")))
            )

        )
      )
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
    
    output$datatable <- renderDataTable({
      
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
    
  }
  
  shinyApp(ui, server, options = list(height = 500))
  
}