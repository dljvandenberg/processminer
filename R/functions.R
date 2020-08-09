## Define Shiny app

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
          sliderInput("duration", "Animation duration", min.time, max.time, default.time),
          selectInput("type", "Animation type", c("relative", "absolute"), "relative"),
          selectInput("sizeAttribute", "Size attribute", c("none", colnames(eventlog)), "none"),
          selectInput("colorAttribute", "Color attribute", c("none", colnames(eventlog)), "none"),
          selectInput("orientation", "Orientation", c("horizontal"="LR", "vertical"="TB"), "horizontal"),
          h4("Selected cases"),
          textOutput("token_selection"),
          h4("Selected activities"),
          textOutput("activity_selection")
        ),
        
        mainPanel(
          width = 10,
          shinycssloaders::withSpinner(processanimaterOutput("process"))
        )
      )
    )
  }
  
  server <- function(session, input, output) {
    
    data <- reactive({
      
      if (input$colorAttribute != "none") {
        attr <- rlang::sym(input$colorAttribute)
        val <- eventlog %>% pull(!!attr)
        if (!(is.character(val) || is.factor(val))) {
          warning("Trying to use a numeric attribute for the token color!")
        }
      }
      
      if (input$sizeAttribute != "none") {
        # This only works for numeric attributes
        attr <- rlang::sym(input$sizeAttribute)
        val <- eventlog %>% pull(!!attr)
        if (!is.numeric(val)) {
          warning("Trying to use a non-numeric attribute for the token size!")
        }
      }
      
      eventlog
      
    })
    
    output$token_selection <- renderText({
      
      paste0(input$process_tokens, ",")
      
    })
    
    output$activity_selection <- renderText({
      
      paste0(input$process_activities, ",")
      
    })
    
    output$process <- renderProcessanimater(expr = {
      graph <- processmapR::process_map(data(), render = F)
      model <- DiagrammeR::add_global_graph_attrs(graph, attr = "rankdir", value = input$orientation, attr_type = "graph")
      if (input$sizeAttribute != "none" && input$colorAttribute != "none") {
        animate_process(data(), model,
                        mode = input$type,
                        legend = "color",
                        mapping = token_aes(color = token_scale(input$colorAttribute, scale = "ordinal", 
                                                                range = RColorBrewer::brewer.pal(5, "YlOrBr")),
                                            size = token_scale(input$sizeAttribute, scale = "linear", range = c(6,10))),
                        duration = input$duration)
      } else if (input$sizeAttribute != "none") {
        animate_process(data(), model,
                        mode = input$type,
                        legend = "size",
                        mapping = token_aes(size = token_scale(input$sizeAttribute, scale = "linear", range = c(6,10))),
                        duration = input$duration)
        
      } else if (input$colorAttribute != "none") {
        animate_process(data(), model,
                        mode = input$type,
                        legend = "color",
                        mapping = token_aes(color = token_scale(input$colorAttribute, scale = "ordinal", range = RColorBrewer::brewer.pal(5, "YlOrBr"))),
                        duration = input$duration)
      } else {
        animate_process(data(), model,
                        mode = input$type,
                        duration = input$duration)
      }
      
    })
    
  }
  
  shinyApp(ui, server, options = list(height = 500))
  
}