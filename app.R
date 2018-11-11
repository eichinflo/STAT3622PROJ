library(shinydashboard)
library(shiny)
library(crayon)
setwd("~/Desktop/STAT3622PROJ-master")
source('cloud.R')
source('timeline.R')


ui <- dashboardPage(
  dashboardHeader(title = "Twitter Troll Analysis App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("Word Timeline", tabName = "wordtl", icon = icon("book")),
      menuItem("Tweet Timeline", tabName = "tweet", icon = icon("twitter-square"))
    )
  ),
  

  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Number of Accounts Deleted", height = 100),
                box(title = "Average Number of Tweets per Account", height = 100),
                box(title = "Date Range of Accounts", height = 100)
              )
      ),
      
      # Word Cloud content
      tabItem(tabName = "wordcloud",
              fluidRow(
              box(title = "options", 
                  sliderInput("freq", "Min. Frequency:", min = 1,  max = 400, value = 100),
                  sliderInput("max", "Max No. of Words:", min = 1,  max = 100,  value = 35),
                  sliderInput('scale1', 'Scale', min = 0, max = 10, value = 3),
                  width = 4
                  ),
              mainPanel(plotOutput("cloud"))
              )
      ),
      
      # Word Timeline content
      tabItem(tabName = "wordtl",
              h2("A timeline for words"),
              box(plotOutput("timeline"))
              ),
      
      # Word Timeline content
      tabItem(tabName = "tweet",
              h2("A timeline for tweets")
      )
    )
  )
)


server = function(input, output, session, v = sorted_word_count) {
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$cloud <- renderPlot({
    wordcloud_rep(rownames(sorted_word_count), 
                  as.integer(sorted_word_count[,1]), 
                  scale=c(input$scale1,1),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Set2"))
  })
  
  output$timeline = renderPlot({
    print(make_freq_plot())
  })
}

shinyApp(ui, server)