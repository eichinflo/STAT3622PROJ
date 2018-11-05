# the server function of the app
server = function(input, output, session, v = sorted_word_count) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # update button
    input$update
    isolate({
      withProgress({
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    wordcloud_rep(rownames(sorted_word_count), 
                  as.integer(sorted_word_count[,1]), 
                  scale=c(input$scale1,1),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Set2"))
  })
}
