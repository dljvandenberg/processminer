source('R/mod_eventlogsummary.R')
source('R/mod_processflow.R')
source('R/ui.R')
source('R/server.R')



### Start app

shinyApp(ui, server, options = list(height = 500))
