library(dplyr)
library(ggplot2)
library(plotly)

# supressing scientific notation for favs.per.tweet calculation

options(scipen=999)

message(red('Reading csv, this may take a while...'))
users = read.csv('data/users.csv')
message(red('Done.'))

# creating new column for creation date of each account in a YYYYMM format
# also removing accounts with NA

mut_users = mutate(users, creation = paste0(substr(created_at, 27, 30),
                                            substr(created_at, 5, 7)))
mut_users = filter(mut_users, creation != "NA NA")
mut_users = mutate(mut_users, creation = paste0(gsub("Jan", "01", creation))) 
mut_users = mutate(mut_users, creation = paste0(gsub("Feb", "02", creation))) 
mut_users = mutate(mut_users, creation = paste0(gsub("Mar", "03", creation))) 
mut_users = mutate(mut_users, creation = paste0(gsub("Apr", "04", creation))) 
mut_users = mutate(mut_users, creation = paste0(gsub("May", "05", creation))) 
mut_users = mutate(mut_users, creation = paste0(gsub("Jun", "06", creation))) 
mut_users = mutate(mut_users, creation = paste0(gsub("Jul", "07", creation))) 
mut_users = mutate(mut_users, creation = paste0(gsub("Aug", "08", creation))) 
mut_users = mutate(mut_users, creation = paste0(gsub("Sep", "09", creation))) 
mut_users = mutate(mut_users, creation = paste0(gsub("Oct", "10", creation))) 
mut_users = mutate(mut_users, creation = paste0(gsub("Nov", "11", creation))) 
mut_users = mutate(mut_users, creation = paste0(gsub("Dec", "12", creation)))
mut_users = filter(mut_users, creation != "NANA")

# calculating average favorites per tweet, filtering out those with some influence
# aka >= 1.0 favorite per tweet

mut_users = mutate(mut_users, favs.per.tweet = (favourites_count / statuses_count))
influential = filter(mut_users, favs.per.tweet >= 1.0, followers_count > 0)

# changing the lang from the abbreviation to the actual word so they appear
# on the legend as the full word

influential = mutate(influential, lang = paste0(gsub("de", "German", lang)))
influential = mutate(influential, lang = paste0(gsub("en", "English", lang)))
influential = mutate(influential, lang = paste0(gsub("ru", "Russian", lang)))

# plotting user information to show relative influence with average favs per tweet

inf = ggplot(influential, aes(favs.per.tweet, followers_count, color = lang)) +
  geom_point(aes(text = paste("User: ", influential$name))) + 
  theme(plot.title = element_text(size = 12),
        legend.title = element_blank()) +
  ggtitle("Relative User Influence") + 
  xlab("Average Favorites per Tweet") + ylab("Followers Count")
  
ggplotly(inf)
