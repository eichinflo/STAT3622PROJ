# the server function of the app

library(wordcloud)

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
}
