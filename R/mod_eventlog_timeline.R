# Shiny Module for displaying events timeline from eventlog

library(dplyr)
library(bupaR)
library(edeaR)
library(processmapR)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)



#################################
## Events Timeline tab UI code ##
#################################
eventsTimelineTabUI <- function(id){
  ns <- NS(id)

  # Events Timeline body contents
  body_events_timeline <- fluidRow(
    box(id = "timeline_box",
        title = "Events Timeline",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        shinycssloaders::withSpinner(plotlyOutput(ns("plotlydottedchart"), height = 700))
    ),
  )
  
  # Show events timeline tab
  tabItem(tabName = "events_timeline", body_events_timeline)
}



#####################################
## Events Timeline tab server code ##
#####################################
eventsTimelineTab <- function(input, output, session, eventlog){
  ns <- session$ns
  
  # Events Timeline diagram
  output$plotlydottedchart <- renderPlotly(expr = {
    
    req(eventlog())
    
    processmapR::plotly_dotted_chart(eventlog())
  })
  
  # Show events_timeline tab in menu
  menuItem(text = "Events Timeline", tabName = "events_timeline", icon = icon("clock"))
}
