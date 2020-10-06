source('R/utils.R')
source('R/mod_table_view.R')
source('R/mod_eventlog_summary.R')
source('R/mod_process_flow.R')
source('R/mod_eventlog_timeline.R')
source('R/ui.R')
source('R/server.R')



### Start app

shinyApp(ui, server, options = list(height = 500))
