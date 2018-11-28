# The UI of the app.

library(shiny)
library(crayon)

ui = fluidPage(
  titlePanel("Russian Troll Tweets v0.2"),
  navbarPage("",
             tabPanel("Cloud",
                      sidebarLayout(
                        # slider sidebar
                        sidebarPanel(
                          sliderInput("freq",
                                      "Min. Frequency:",
                                      min = 100,  max = 4000, value = 1000),
                          sliderInput("max",
                                      "Max No. of Words:",
                                      min = 10,  max = 500,  value = 100),
                          sliderInput('scale1',
                                      'Scale', min = 0, max = 10, value = 3)
                        ),
                        # word cloud
                        mainPanel(plotOutput("cloud")))),
              tabPanel("Timeline",
                       plotOutput("timeline"),
                         sidebarPanel(
                           selectizeInput(
                             'words', 'Keywords', 
                             choices = names(sort(rowSums(counts_by_month_as_matrix), decreasing = TRUE)[1:1000]), multiple = TRUE
                           ),
                           checkboxInput("frequency", label = "Frequency Plot", value = FALSE)))
             )
  )


