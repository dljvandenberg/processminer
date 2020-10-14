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
  
  # Helper texts
  timeline_view_help_text <- "
  <p>The <b>timeline view</b> shows all events over time (x-axis), for each case (y-axis). Each dot represents an <b>event</b>.</p>
  <p>The events of a single <b>case</b> are shown horizontally next to each other.</p>
  <p>The color of each dot corresponds to the <b>type of activity</b> (see legend).</p>
  <p>You can select an area of the diagram by dragging the mouse.</p>"

  # Events Timeline body contents
  body_events_timeline <- fluidRow(
    box(id = "timeline_box",
        title = "Events Timeline",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        shinycssloaders::withSpinner(plotlyOutput(ns("plotlydottedchart"), height = 700) %>% 
                                       shinyhelper::helper(type = "inline", title = "Timeline view", content = timeline_view_help_text, size = 'l'))
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
  menuItem(text = "Events timeline", tabName = "events_timeline", icon = icon("clock"))
}
