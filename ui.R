# The UI of the app.

ui = fluidPage(
  titlePanel("Russian Troll Tweets v0.2"),
  navbarPage("",
             tabPanel("Cloud",
                      sidebarLayout(
                        # slider sidebar
                        sidebarPanel(
                          sliderInput("freq",
                                      "Min. Frequency:",
                                      min = 1,  max = 400, value = 100),
                          sliderInput("max",
                                      "Max No. of Words:",
                                      min = 1,  max = 100,  value = 35),
                          sliderInput('scale1',
                                      'Scale', min = 0, max = 10, value = 3)
                        ),
                        # word cloud
                        mainPanel(plotOutput("cloud")))),
              tabPanel("Timeline", plotOutput("timeline"))
             )
  )


