library('tidyverse')

############ RICKY ################


# Break up logical sections like the line at the top and write comments with a single # 

# ie 


############ BOX PLOT ################

# I think we should do a box plot like so

#Made New Twitter Data Set To Provide Context, Included Numbers of Tweets and Number of Followers

table(wine_data$taster_twitter_handle)

TwitterData <- read_excel("data/input/TwitterData.xlsx")

ggplot(TwitterData, mapping = aes(x = number_of_tweets, y = number_of_followers, color = taster_twitter_handle)) + geom_point() + theme_fivethirtyeight() + ggtitle("Twitter Chart")

                                    