library('tidyverse')

############ RICKY ################


# Break up logical sections like the line at the top and write comments with a single # 

# ie 


############ BOX PLOT ################

# I think we should do a box plot like so

#Made New Twitter Data Set To Provide Context, Included Numbers of Tweets and Number of Followers

table(wine_data$taster_twitter_handle)

TwitterData <- read_excel("data/input/TwitterData.xlsx")

ggplot(TwitterData, mapping = aes(x = number_of_tweets, y = number_of_followers, color = taster_twitter_handle)) + geom_point() + ggtitle("Twitter Chart") + labs(x = "number of tweets", y = "number of followers")

mean(wine_data$price, wine_data$taster_twitter_handle)


#Plots

ggplot(wine_data, aes(x = point_cat, y = price)) +
  geom_bar(color = "purple", stat = "identity") + ggtitle("Point_Cat and Price")

ggplot(wine_data, aes(x = price, y = country_lump, color = point_cat)) +
  geom_jitter() + ggtitle("Price and Country Colored by Point_Cat")

ggplot(wine_data, aes(taster_review_count, fill = taster_avg_points)) + geom_density(position = "stack", color = "purple") + ggtitle("Density Plot of Twitter Data")

                                    