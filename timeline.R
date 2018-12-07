# code for the timeline
library(crayon)
library(dplyr)
library(ggplot2)

# plot tweet frequency per month
make_freq_plot = function(words,
                          counts=counts_by_month_as_matrix,
                          trump_counts = counts_by_month_as_matrix_trump,
                          clinton_counts = counts_by_month_as_matrix_clinton,
                          frequency=FALSE,
                          trump=FALSE,
                          clinton=FALSE,
                          dates=FALSE) {
  
  if (length(words) == 0) {
    #TODO: freq
    # no word is selected
    data = data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"))
    return(ggplot(data, aes(x = month)) +
           theme_minimal() + theme(axis.text.x = element_text(colour="grey20",size=12, angle = 90, hjust = 1),
                                    axis.text.y = element_text(colour="grey20",size=12),  
                                    axis.title.x = element_text(colour="grey20",size=17, margin=margin(t=20)),
                                    axis.title.y = element_text(colour="grey20",size=17, margin=margin(r=20)),
                                    legend.background = element_rect(fill="white", 
                                                                     size=0.2, linetype = "blank"),
                                    legend.title = element_blank()) +
                                    ylab('Number of Tweets') +
                                    xlab('Month'))
  }

  # store data in a data frame that suits the ggplot2 diva
  data = data.frame()
  data_trump = data.frame()
  data_clinton = data.frame()
  
  if (!frequency) {
    # absolute plot
    for (word in words) {
      data = rbind(data, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                    count = as.vector(counts_by_month_as_matrix[word,]), wrd = word))
      if (word %in% rownames(counts_by_month_as_matrix_trump)) {
        data_trump = rbind(data_trump, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                      count = as.vector(counts_by_month_as_matrix_trump[word,]), wrd = word))
      } else {
        data_trump = rbind(data_trump, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                                  count = numeric(39), wrd = word))
      }
      if (word %in% rownames(counts_by_month_as_matrix_clinton)) {
        data_clinton = rbind(data_clinton, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                      count = as.vector(counts_by_month_as_matrix_clinton[word,]), wrd = word))
      } else {
        data_clinton = rbind(data_clinton, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                                      count = numeric(39), wrd = word))
      }
    }
  } else {
    # frequency plot
    for (word in words) {
      data = rbind(data, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                    count = as.vector(counts_by_month_as_matrix[word,]) / sum(as.vector(counts_by_month_as_matrix[word,])),
                                    wrd = word))
      
      if (word %in% rownames(counts_by_month_as_matrix_trump)) {
        data_trump = rbind(data_trump, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                                  count = as.vector(counts_by_month_as_matrix_trump[word,]) / sum(as.vector(counts_by_month_as_matrix_trump[word,])),
                                                  wrd = word))
      } else {
        data_trump = rbind(data_trump, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                                  count = numeric(39), wrd = word))
      }
      if (word %in% rownames(counts_by_month_as_matrix_clinton)) {
        data_clinton = rbind(data_clinton, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                                     count = as.vector(counts_by_month_as_matrix_clinton[word,]) / sum(as.vector(counts_by_month_as_matrix_clinton[word,])),
                                                     wrd = word))
      } else {
        data_clinton = rbind(data_clinton, data.frame(month = c(substr(as.vector(colnames(counts))[1:38], 1, 7), "2017-10"), 
                                                      count = numeric(39), wrd = word))
      }
    }
  }
  
  # the actual timeline plot
  if (!(trump || clinton)) {
    plot = ggplot(data, aes(x=factor(month))) + 
      geom_line(data = data, aes(y=count, group=wrd, color=wrd), linetype='solid') +
      theme_minimal() + theme(axis.text.x = element_text(colour="grey20",size=12, angle = 90, hjust = 1), 
                              axis.text.y = element_text(colour="grey20",size=12),  
                              axis.title.x = element_text(colour="grey20",size=17,  margin=margin(t=20)),
                              axis.title.y = element_text(colour="grey20",size=17, margin=margin(r=20)),
                              legend.background = element_rect(fill="white", 
                                                               size=0.2, linetype = "blank"),
                              legend.title = element_blank(),
                              legend.position = "right") +
      ylab('Number of Tweets') + xlab('Month')
  } else if (trump && !clinton) {
    plot = ggplot(data, aes(x=factor(month))) + 
      geom_line(data = data, aes(y=count, group=wrd, color=wrd), linetype='solid') +
      geom_line(data = data_trump, aes(y=count, group=wrd, color=wrd), linetype='dashed') +
      theme_minimal() + theme(axis.text.x = element_text(colour="grey20",size=12, angle = 90, hjust = 1), 
                              axis.text.y = element_text(colour="grey20",size=12),  
                              axis.title.x = element_text(colour="grey20",size=17,  margin=margin(t=20)),
                              axis.title.y = element_text(colour="grey20",size=17, margin=margin(r=20)),
                              legend.background = element_rect(fill="white", 
                                                               size=0.2, linetype = "blank"),
                              legend.title = element_blank(),
                              legend.position = "right") +
      ylab('Number of Tweets') + xlab('Month')
  } else if (!trump && clinton) {
    plot = ggplot(data, aes(x=factor(month))) + 
      geom_line(data = data, aes(y=count, group=wrd, color=wrd), linetype='solid') +
      geom_line(data = data_clinton, aes(y=count, group=wrd, color=wrd), linetype='dotted') +
      theme_minimal() + theme(axis.text.x = element_text(colour="grey20",size=12, angle = 90, hjust = 1), 
                              axis.text.y = element_text(colour="grey20",size=12),  
                              axis.title.x = element_text(colour="grey20",size=17,  margin=margin(t=20)),
                              axis.title.y = element_text(colour="grey20",size=17, margin=margin(r=20)),
                              legend.background = element_rect(fill="white", 
                                                               size=0.2, linetype = "blank"),
                              legend.title = element_blank(),
                              legend.position = "right") +
      ylab('Number of Tweets') + xlab('Month') 
  } else {
    plot = ggplot(data, aes(x=factor(month))) + 
      geom_line(data = data, aes(y=count, group=wrd, color=wrd), linetype='solid') +
      geom_line(data = data_trump, aes(y=count, group=wrd, color=wrd), linetype='dashed') +
      geom_line(data = data_clinton, aes(y=count, group=wrd, color=wrd), linetype='dotted') +
      theme_minimal() + theme(axis.text.x = element_text(colour="grey20",size=12, angle = 90, hjust = 1), 
                              axis.text.y = element_text(colour="grey20",size=12),  
                              axis.title.x = element_text(colour="grey20",size=17,  margin=margin(t=20)),
                              axis.title.y = element_text(colour="grey20",size=17, margin=margin(r=20)),
                              legend.background = element_rect(fill="white", 
                                                               size=0.2, linetype = "blank"),
                              legend.title = element_blank(),
                              legend.position = "right") +
      ylab('Number of Tweets') + xlab('Month')
  }

  return(plot)
}

