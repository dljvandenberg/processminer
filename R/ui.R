# User-interface definition of ProcessMiner Shiny web application

library(dplyr)
library(shiny)
library(shinydashboard)
library(shinycssloaders)


#####################
## JavaScript code ##
#####################

javascript_head <- tags$head(tags$script('
      // Define function to set height of "process_box", "process", "timeline_box" and "plotlydottedchart"
      // See https://stackoverflow.com/questions/56965843/height-of-the-box-in-r-shiny
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 100;

        $("#process_box").height(boxHeight);
        $("#process_flow-process").height(boxHeight - 30);
        
        $("#timeline_box").height(boxHeight);
        $("#events_timeline-plotlydottedchart").height(boxHeight - 30);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    '))


###################################
## Components for dashboard body ##
###################################

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


#######################################
## Bring the above UI parts together ##
#######################################

ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = "ProcessMiner"),
    dashboardSidebar(sidebarMenuOutput("sidebarmenu")),
    dashboardBody(
      javascript_head,
      tabItems(
        # Available dashboard body content
        tabItem(tabName = "data_upload", body_data_upload),
        tabItem(tabName = "example_dataset", body_example_dataset),
        tableViewTabUI(id = "table_view"),
        eventlogSummaryTabUI(id = "summary_stats"),
        processFlowTabUI(id = "process_flow"),
        eventsTimelineTabUI(id = "events_timeline"),
        tabItem(tabName = "about_this_app", body_about_this_app)
      )
    )
  )
}
