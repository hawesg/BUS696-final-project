################### LOAD DATA ###################

wine_data <-
  read.csv(here::here("data", "input", "winemag-data-130k-v2.csv"))
colors <- read.csv(here::here("data", "input", "wine-colors.csv"))
twitter_stats <- read.csv(here::here("data", "input", "twitter-data.csv"))

wine_data <- dplyr::left_join(wine_data, colors, by = "variety")

# Add in twitter stats  ---------------------------------------------------

wine_data_with_twitter_data <- dplyr::left_join(wine_data, twitter_stats, by = "taster_twitter_handle")

wine_data <- wine_data_with_twitter_data

