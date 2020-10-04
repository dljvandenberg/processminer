### Start app ###

source('ui.R')
source('server.R')

shinyApp(ui, server, options = list(height = 500))
