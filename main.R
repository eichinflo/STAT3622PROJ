# the main function
library(shiny)
library(crayon)
source('counting.R')
# source('cloud.R')
source('server.R')
source('ui.R')
source('timeline.R')


message(red('To see the app, copy the below link to your browser.'))
shinyApp(ui = ui, server = server)

# TODO: timeline parameter interactive (scale type, show compare)
#       (total sums for timeline)
#       list of 'botted' features, that appear rapidly seeable #stopislam #islamkills...
#       compare dataset of normal users/donald trump
#       layout timeline
