# code for the timeline
library(crayon)
library(dplyr)
library(ggplot2)

message(red('Reading csv, this may take a while...'))
tweets = read.csv('data/tweets.csv')
message(red('Done.'))

# plot tweet frequency per month
make_freq_plot = function() {
  tweets = mutate(tweets, month = as.Date(paste0(substr(created_str, 1, 4),
                                  '-', substr(created_str, 6, 7),
                                  '-', '01')))
  # the actual timeline plot
  plot = ggplot(tweets, aes(x = month)) + geom_bar(bins = 48) + 
    theme_minimal() + theme(axis.text.x = element_text(colour="grey20",size=12, angle = 90, hjust = 1), 
                            axis.text.y = element_text(colour="grey20",size=12),  
                            axis.title.x = element_text(colour="grey20",size=17),
                            axis.title.y = element_text(colour="grey20",size=17)) +
    scale_x_date(name = 'Months', date_breaks = "1 month") +
    ylab('Number of Tweets')
  return(plot)
}

