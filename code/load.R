################### LOAD DATA ###################
wine_data <-
  read.csv(here("data", "input", "winemag-data-130k-v2.csv"))
colors <- read.csv(here("data", "input", "wine-colors.csv"))
twitter_stats <- read.csv(here("data", "input", "twitter-data.csv"))
dim(wine_data)

glimpse(colors)
str(twitter_stats)
glimpse(twitter_stats)

#Export to csv for analysis
# x <- wine_data %>% select(taster_twitter_handle)
# x <- unique(x)
# x
# write.table(x, file = "tw.csv", sep = ",", col.names = NA,
#             qmethod = "double")

# Add color
# wine_data_with_color <- left_join(wine_data, colors, by = "variety")
wine_data <- left_join(wine_data, colors, by = "variety")

summary(wine_data)


# Add in twitter stats  ---------------------------------------------------

wine_data_with_twitter_data <- left_join(wine_data, twitter_stats, by = "taster_twitter_handle")

# glimpse(wine_data_with_twitter_data)
# 
# tw_nas <- wine_data_with_twitter_data%>% filter(is.na(taster_gender))
# dim(tw_nas)
# unique(tw_nas$taster_name)
# 
# genders_na <- as.character(unique(tw_nas$taster_name))
# 
# view(genders_na)

#wine <- merge(wine_data, twitter_stats, by = "taster_twitter_handle")

# nrow(wine_data)
# #[1] 129833
# nrow(wine_data_with_color)
# #[1] 126407
# nrow(wine_data) - nrow(wine_data_with_color)
# # 3426 records missing
# 
# # List of records i am not getting merged by ID
# setdiff(wine_data$X, wine_data_with_color$X)
# 
# wine_data <- wine_data_with_color

# Add in twitter stats  ---------------------------------------------------

wine_data_with_twitter_data <- left_join(wine_data, twitter_stats, by = "taster_twitter_handle")

summary(wine_data_with_twitter_data)


wine_data_original <- wine_data

wine_data <- wine_data_with_twitter_data
