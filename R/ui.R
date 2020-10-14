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

        var boxHeightGeneral = window_height - header_height - 100;
        var boxHeightProcessFlow = boxHeightGeneral - 50;
        
        $("#process_box").height(boxHeightProcessFlow);
        $("#process_flow-process").height(boxHeightProcessFlow - 30);
        
        $("#timeline_box").height(boxHeightGeneral);
        $("#events_timeline-plotlydottedchart").height(boxHeightGeneral - 30);
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
        loadEventLogTabUI(id = "data_upload", type = "data_upload"),
        loadEventLogTabUI(id = "example_dataset", type = "example_dataset"),
        tableViewTabUI(id = "table_view"),
        eventlogSummaryTabUI(id = "summary_stats"),
        processFlowTabUI(id = "process_flow"),
        eventsTimelineTabUI(id = "events_timeline"),
        tabItem(tabName = "about_this_app", body_about_this_app)
      )
    )
  )
}
