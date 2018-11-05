# helper code for the word cloud
library(shiny)
library(tm)
library(wordcloud)
library(stringr)

# this method reads the data, and writes the text of all tweets
# to a .txt file (we might think of changing the data structure here)
write_tweet_corpus = function() {
  tweets = read.csv('data/tweets.csv')
  # get only the text column, as this contains all we need for now
  text_only = tweets['text']
  # this is going to be a string of all tweets
  text_as_string = ""
  for (tweet in text_only) {
    text_as_string = paste(text_as_string, as.character(tweet))
  }
  file = file('data/corpus.txt')
  write(x = text_as_string, file = file)
  close(file)
}

# from a string text, extract all words and store them as character type
# into a list
get_words = function(text) {
  # also use hashtag later on
  words = str_extract_all(text, '[A-Za-z]+')
  return(words)
}

# this rather messy method counts all the word occurences in corpus.txt
# if we want to use this, we should definitely think of alternatives
write_sorted_word_counts = function() {
  word_count = list()
  lines = get_words(readLines("data/corpus.txt", encoding="UTF-8"))
  for (line in lines) {
    if (length(line) > 0) {
      for (word in line[[1]]) {
        # better criterion here or filtering of unnecessary words in corpus
        if (nchar(word) > 3) {
          if (is.null(word_count[[word]])) {
            word_count[word] = 1
          } else {
            word_count[word] = word_count[[word]] + 1
          }
        }
      }
    }
  }
  
  # exclude uninteresting words
  exclude = c('https', 'http', 'Then', 'Where', 'There', 'Here',
              'They', 'That', 'When', 'What', 'Just', 'This', 'Every')
  for (word in exclude) {
    word_count[word] = 0
  }
  
  # sort according to frequency and convert to a matrix
  sorted_word_count = as.matrix(make_sorted(word_count))
  
  # store the data object, so we don't have to run 
  # the above code again
  save(sorted_word_count, file = 'data/counts.txt')
}

# sort the words after their word counts
make_sorted = function(word_count){
  return(sort(unlist(word_count), decreasing = TRUE))
}

if (!file.exists('data/corpus.txt')) {
  print('Writing corpus.txt - this can take a while...')
  write_tweet_corpus() # execute only once, the text will be saved in a corpus.txt file
  print('Done.')
}

if (!file.exists('data/counts.txt')) {
  print('Counting words...')
  write_sorted_word_counts()
}
load('data/counts.txt')