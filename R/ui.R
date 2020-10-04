# User-interface definition of ProcessMiner Shiny web application

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
        p("It was created by Dennis van den Berg and uses the bupaR process mining library in R, with a UI built in R Shiny."),
        p("Its current status is: experimental"),
        p("Source code: ", a(href="https://github.com/dljvandenberg/processminer", "https://github.com/dljvandenberg/processminer"))
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
        $("#process_flow_1-process").height(boxHeight - 30);
        
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
        eventlogSummaryUI(id = "summary_stats_1"),
        processFlowUI(id = "process_flow_1"),
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
