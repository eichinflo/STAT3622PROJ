# the main function
source('cloud.R')
source('server.R')
source('ui.R')

print('To see the app, copy the below link to your browser.')
shinyApp(ui = ui, server = server)
