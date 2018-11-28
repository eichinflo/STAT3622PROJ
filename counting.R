# code to create word counts table
# table consists of month columns and word rows

library(crayon)
library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(jsonlite)

write_counts = function() {
  # write a new counts.txt
  message(red('Reading csv, this may take a while ...'), appendLF = FALSE)
  tweets = read.csv('data/tweets.csv')
  message(red(' done.'))
  message(red('Counting words ... '), appendLF = FALSE)
  tweets = mutate(tweets, month = as.Date(paste0(substr(created_str, 1, 4),
                                                 '-', substr(created_str, 6, 7),
                                                 '-', '01')))
  
  tweets_by_month = split(tweets, f = tweets$month)
  months = t(distinct(tweets, month) %>% arrange(month))
  # define handy structure (columns = words, rows = months)
  counts_by_month = NULL
  ct = 0
  for (i in 1:length(months)) {
    tweets_for_this_month = filter(tweets, month == months[i])
    text_only = tweets_for_this_month['text']
    
    # text_as_string is a string of all tweets
    text_as_string = ""
    for (tweet in text_only) {
      text_as_string = paste(text_as_string, as.character(tweet), sep = " ")
    }
    
    # words should be at least 4 characters long and may be preceded by # (Hashtags)
    words = as.vector(str_extract_all(text_as_string, '#?[A-Za-z\']{4,}', simplify = TRUE))
    # this creates a term frequency count for all words in this month
    counts_for_this_month = termFreq(words)
    # we need to initialize counts_by_months correctly
    if (ct == 0) {
      counts_by_month = counts_for_this_month
    } else {
      # this implicitly returns a TermDocumentMatrix
      counts_by_month = c(counts_by_month, counts_for_this_month)
    }
    ct = ct + 1
  }
  
  counts_by_month_as_matrix = as.matrix(counts_by_month)
  colnames(counts_by_month_as_matrix) = months
  message(red('done.'))
  message(red('Saving to data/counts.txt ...'), appendLF = FALSE)
  
  exclude = c('https', 'http', 'then', 'where', 'there', 'here',
              'they', 'that', 'when', 'what', 'just', 'this', 'every')
  # exclude words for wordcloud
  for (word in stopwords()) {
    if (word %in% rownames(counts_by_month_as_matrix)) {
      counts_by_month_as_matrix[word,] = numeric(39)
    }
  }
  for (word in exclude) {
    counts_by_month_as_matrix[word,] = numeric(39)
  }
  
  save(counts_by_month_as_matrix, counts_by_month, file = 'data/counts.txt')
  message(red(' done.'))
}

write_trump_counts = function() {
  trump_tweets = fromJSON('data/trump.json') %>%
    mutate(created_str = as.character(created_at)) %>%
    mutate(month = as.Date(paste0(substr(created_at, 27, 30),
                                  '-', substr(created_at, 5, 7),
                                  '-', '01'), '%Y-%b-%d')) %>%
    filter(month < '2017-10-01', month > '2014-06-01')
  
  tweets_by_month = split(trump_tweets, f = trump_tweets$month)
  months = t(distinct(trump_tweets, month) %>% arrange(month))
  # define handy structure (columns = words, rows = months)
  counts_by_month = NULL
  ct = 0
  for (i in 1:length(months)) {
    tweets_for_this_month = filter(trump_tweets, month == months[i])
    text_only = tweets_for_this_month['text']
    
    # text_as_string is a string of all tweets
    text_as_string = ""
    for (tweet in text_only) {
      text_as_string = paste(text_as_string, as.character(tweet), sep = " ")
    }
    
    # words should be at least 4 characters long and may be preceded by # (Hashtags)
    words = as.vector(str_extract_all(text_as_string, '#?[A-Za-z\']{4,}', simplify = TRUE))
    # this creates a term frequency count for all words in this month
    counts_for_this_month = termFreq(words)
    # we need to initialize counts_by_months correctly
    if (ct == 0) {
      counts_by_month = counts_for_this_month
    } else {
      # this implicitly returns a TermDocumentMatrix
      counts_by_month = c(counts_by_month, counts_for_this_month)
    }
    ct = ct + 1
  }
  
  counts_by_month_as_matrix = as.matrix(counts_by_month)
  colnames(counts_by_month_as_matrix) = months

  exclude = c('https', 'http', 'then', 'where', 'there', 'here',
              'they', 'that', 'when', 'what', 'just', 'this', 'every')
  # exclude words for wordcloud
  for (word in stopwords()) {
    if (word %in% rownames(counts_by_month_as_matrix)) {
      counts_by_month_as_matrix[word,] = numeric(39)
    }
  }
  for (word in exclude) {
    counts_by_month_as_matrix[word,] = numeric(39)
  }
  
  counts_by_month_trump = counts_by_month
  counts_by_month_as_matrix_trump = counts_by_month_as_matrix
  save(counts_by_month_as_matrix_trump, counts_by_month_trump, file = 'data/trump_counts.txt')
}

write_clinton_counts = function() {
  clinton_tweets = fromJSON('data/clinton.json') %>%
    mutate(created_str = as.character(created_at)) %>%
    mutate(month = as.Date(paste0(substr(created_at, 27, 30),
                                  '-', substr(created_at, 5, 7),
                                  '-', '01'), '%Y-%b-%d')) %>%
    filter(month < '2017-10-01', month > '2014-06-01')
  
  tweets_by_month = split(clinton_tweets, f = clinton_tweets$month)
  months = t(distinct(clinton_tweets, month) %>% arrange(month))
  # define handy structure (columns = words, rows = months)
  counts_by_month = NULL
  ct = 0
  for (i in 1:length(months)) {
    tweets_for_this_month = filter(clinton_tweets, month == months[i])
    text_only = tweets_for_this_month['text']
    
    # text_as_string is a string of all tweets
    text_as_string = ""
    for (tweet in text_only) {
      text_as_string = paste(text_as_string, as.character(tweet), sep = " ")
    }
    
    # words should be at least 4 characters long and may be preceded by # (Hashtags)
    words = as.vector(str_extract_all(text_as_string, '#?[A-Za-z\']{4,}', simplify = TRUE))
    # this creates a term frequency count for all words in this month
    counts_for_this_month = termFreq(words)
    # we need to initialize counts_by_months correctly
    if (ct == 0) {
      counts_by_month = counts_for_this_month
    } else {
      # this implicitly returns a TermDocumentMatrix
      counts_by_month = c(counts_by_month, counts_for_this_month)
    }
    ct = ct + 1
  }
  
  counts_by_month_as_matrix = as.matrix(counts_by_month)
  colnames(counts_by_month_as_matrix) = months
  
  exclude = c('https', 'http', 'then', 'where', 'there', 'here',
              'they', 'that', 'when', 'what', 'just', 'this', 'every')
  # exclude words for wordcloud
  for (word in stopwords()) {
    if (word %in% rownames(counts_by_month_as_matrix)) {
      counts_by_month_as_matrix[word,] = numeric(39)
    }
  }
  for (word in exclude) {
    counts_by_month_as_matrix[word,] = numeric(39)
  }
  counts_by_month_as_matrix_clinton = counts_by_month_as_matrix
  counts_by_month_clinton = counts_by_month
  save(counts_by_month_as_matrix_clinton, counts_by_month_clinton, file = 'data/clinton_counts.txt')
}

if (!file.exists('data/counts.txt')) {
  write_counts()
}

load('data/counts.txt')
load('data/trump_counts.txt')
load('data/clinton_counts.txt')
# TODO: make methods and do same thing for hillary