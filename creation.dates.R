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

#bar chart to show number of accounts created per month

counter = data.frame(Month = "Month", Count = "Count")

for (x in mut_users$creation) {
  each_month = filter(mut_users, creation == x)
  each_month_df = data.frame(Month = as.character(x), Count = as.character(tally(each_month)$n))
  counter = rbind(counter, each_month_df)
}

counter = counter[-c(1),]
counter = filter(counter, Month != "NA-NA")
counter = transform(counter, Month = as.integer(paste(Month)))
counter = counter[order(counter$Month),]
counter = transform(counter, Month = as.character(paste(Month)))

create = ggplot(counter, aes(x = Month)) +
  geom_bar(colour = "black", fill = "plum") +
  ylab("Count") +
  ggtitle("Creation Dates of Deleted Accounts") +
  theme(axis.text.x = element_text(colour="grey20",size=8, angle = 90, hjust = 1))
ggplotly(create)
