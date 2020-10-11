# Server logic of ProcessMiner Shiny web application

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



server <- function(session, input, output) {
    
    #####################
    ## Reactive values ##
    #####################
    
    # Eventlog, either from processed rawdata() or from loading example evenlog
    reactivevalues <- reactiveValues(eventlog = NULL)

    
    ########################
    ## Render UI contents ##
    ########################
    
    output$sidebarmenu <- renderMenu({
        
        # NULL eventlog corresponds to to data loaded. In case of no data loaded, show less menu items
        data_loaded <- !is.null(reactivevalues$eventlog)
        
        # Define these menuitems once, in order to use them in the conditional part below
        menuitem_dataload <- menuItem(text = "Load data", icon = icon("database"), startExpanded = TRUE,
                                      callModule(loadEventLogTab, "example_dataset", myreactivevalues = reactivevalues, type = "example_dataset", selected = TRUE),
                                      callModule(loadEventLogTab, "data_upload", myreactivevalues = reactivevalues, type = "data_upload")
        )
        menuitem_about <- menuItem(text = "About ProcessMiner", tabName = "about_this_app", icon = icon("info-circle"))
        
        if(data_loaded){
            # Show extended menu when data has been loaded
            sidebarMenu(
                menuitem_dataload,
                callModule(tableViewTab, "table_view", eventlog = reactive(reactivevalues$eventlog)),
                callModule(eventlogSummaryTab, "summary_stats", myeventlog = reactive(reactivevalues$eventlog)),
                callModule(processFlowTab, "process_flow", eventlog = reactive(reactivevalues$eventlog)),
                callModule(eventsTimelineTab, "events_timeline", eventlog = reactive(reactivevalues$eventlog)),
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
    
}