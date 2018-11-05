# the main function
library(shiny)
library(crayon)
source('cloud.R')
source('server.R')
source('ui.R')
source('timeline.R')

message(blue('To see the app, copy the below link to your browser.'))
shinyApp(ui = ui, server = server)

# TODO: Make timeline interactive and make more information avaliable for
#       plotting (creation dates of accounts, word frequency per month,
#                 number of retweets, etc etc.)