# code for the timeline
library(crayon)
library(dplyr)
library(ggplot2)

# plot tweet frequency per month
make_freq_plot = function(words, counts=counts_by_month_as_matrix, frequency=FALSE) {
  
  if (length(words) == 0) {
    #TODO: freq
    # no word is selected
    data = data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"))
    return(ggplot(data, aes(x = month)) +
           theme_minimal() + theme(axis.text.x = element_text(colour="grey20",size=12, angle = 90, hjust = 1),
                                    axis.text.y = element_text(colour="grey20",size=12),  
                                    axis.title.x = element_text(colour="grey20",size=17),
                                    axis.title.y = element_text(colour="grey20",size=17),
                                    legend.background = element_rect(fill="white", 
                                                                     size=0.2, linetype = "blank"),
                                    legend.title = element_blank()) +
             ylab('Number of Tweets')) + xlab('Month')
  }

  # store data in a data frame that suits the ggplot2 diva
  data = data.frame()
  if (!frequency) {
    # absolute plot
    for (word in words) {
      data = rbind(data, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                    count = as.vector(counts_by_month_as_matrix[word,]), wrd = word))
    }
  } else {
    # frequency plot
    for (word in words) {
      data = rbind(data, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                    count = as.vector(counts_by_month_as_matrix[word,]) / sum(as.vector(counts_by_month_as_matrix[word,])),
                                    wrd = word))
    }
  }
  
  # the actual timeline plot
  plot = ggplot(data, aes(x=factor(month))) + 
    geom_line(data = data, aes(y=count, group=wrd, color=wrd)) +
    theme_minimal() + theme(axis.text.x = element_text(colour="grey20",size=12, angle = 90, hjust = 1), 
                            axis.text.y = element_text(colour="grey20",size=12),  
                            axis.title.x = element_text(colour="grey20",size=17),
                            axis.title.y = element_text(colour="grey20",size=17),
                            legend.background = element_rect(fill="white", 
                                                             size=0.2, linetype = "blank"),
                            legend.title = element_blank(),
                            legend.position = "right") +
    ylab('Number of Tweets') + xlab('Month')

  return(plot)
}

