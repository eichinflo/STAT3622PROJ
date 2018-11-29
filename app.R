library(shinydashboard)
library(shiny)
library(crayon)
library(plotly)
setwd("~/Desktop/STAT3622PROJ-master")
source('cloud.R')
source('timeline.R')
source('counting.R')
source('creation.dates.R')
source('user.influence.R')

ui <- dashboardPage(
  dashboardHeader(title = "Twitter Troll Analysis App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("Word Usage Timeline", tabName = "wordtl", icon = icon("book")),
      menuItem("User Analysis", tabName = "tweet", icon = icon("twitter-square"))
    )
  ),
  

  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBox(454, "Number of Accounts Deleted", icon = icon("user-times"),
                     color = "light-blue"),
                valueBox(448, "Average Number of Tweets per Account", 
                   icon = icon("trash"), color = "red")
              ),
              fluidRow(
                valueBox("Jul 14, 2014", "Date of First Tweet",
                    icon = icon("calendar"), color = "green"),
                valueBox("Sep 26, 2017", "Date of Last Tweet",
                         icon = icon("calendar"), color = "yellow")
              )
      ),
      
      # Word Cloud content
      tabItem(tabName = "wordcloud",
              fluidRow(
              box(title = "Cloud Options", 
                  sliderInput("freq", "Min. Frequency:", min = 1,  max = 400, value = 100),
                  sliderInput("max", "Max No. of Words:", min = 1,  max = 100,  value = 35),
                  sliderInput('scale1', 'Scale', min = 0, max = 10, value = 3),
                  width = 4
                  ),
              mainPanel(plotOutput("cloud"))
              ),
              fluidRow(
                box("By changing the parameters on the sliders, you can view fewer or greater popular words within our set of tweets.
                    The coloration of each word is determined by the number of times that it was used. From greatest to least amount of times used, 
                    the words show as grey, pink, blue, orange, and green. This magnitude of usage is also represented by the size of each word.", width = 100)
              )
      ),
      
      # Word Timeline content
      tabItem(tabName = "wordtl",
              fluidRow(
                box("Enter a word of your choice (that appears in the set of tweets) in to the box below. Then you can overlay multiple word graphs in order
                    to easily compare. If counts are extremely different, you can change the graphs to frequency to get them on a similar scale. Below are graphs of 
                    the same trends in Hillary Clinton and Donald Trump's tweets in order to identify any correlation. 
                    notes: #islamkills #stopislam #thingsmoretrustedthanhillary. in march 2016 trump made a speech that said 'islam hates us'", width = 100)
              ),
              fluidRow(
              box(plotOutput("timeline"), width = 100)
              ),
              fluidRow(
                box(selectizeInput(
                  'words', 'Keywords', 
                  choices = names(sort(rowSums(counts_by_month_as_matrix), decreasing = TRUE)[1:1000]), multiple = TRUE
                ),
                checkboxInput("frequency", label = "Frequency Plot", value = FALSE),
                h5('Compare to twitter activity of...'),
                checkboxInput("trump", label = "Trump (dashed line)", value = FALSE),
                checkboxInput("clinton", label = "Clinton (dotted line)", value = FALSE),
                h5('(Frequency plot recommended)'))
              )
              ),
      
      # User Analysis content
      tabItem(tabName = "tweet",
              fluidRow(
                box(selectInput("graph", "Graph:",
                                c("User Creation" = "a",
                                  "User Influence" = "b")))
              ),
              fluidRow(
                box(plotlyOutput("usergraph"), width = 100
                )
              ),
              fluidRow(
                box("On this page, we created two graphs in order to analyze some of the user details of the data set. We first made a histogram of creation
                    dates of the accounts deleted. The next graph plots average favorites per tweet against follower count of each account. The dots are colored
                    by the language that the account settings are in. These are in plotly, so hovering over points will give you more info.
                    notes: avg fav/tweet of over 1, interesting that a lot of these lie in the lower left corner, not a huge influential pull
                    interesting that a lot of the accoutns were made in 2013, 2014 when trump didn't announce candidacy until june 2015", width = 100)
              )
      )
    )
  )
)


server = function(input, output, session, counts_by_month = counts_by_month_as_matrix,
                  sums_of_counts = sort(rowSums(counts_by_month_as_matrix), decreasing = TRUE)) {
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$cloud <- renderPlot({
    wordcloud_rep(names(sums_of_counts), 
                  sums_of_counts, 
                  scale=c(input$scale1,1),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Set2"))
  })
  
  output$timeline = renderPlot({
    make_freq_plot(input$words, 
                   counts = counts_by_month_as_matrix, 
                   frequency = input$frequency, 
                   trump = input$trump,
                   clinton = input$clinton)
  })
  
  output$usergraph = renderPlotly({
    if (input$graph == "a") a else b
  })
  
}


shinyApp(ui, server)