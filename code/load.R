################### LOAD DATA ###################
wine_data <-
  read.csv(here("data", "input", "winemag-data-130k-v2.csv"))
colors <- read.csv(here("data", "input", "wine-colors.csv"))
twitter_stats <- read.csv(here("data", "input", "twitter-data.csv"))
dim(wine_data)

glimpse(colors)
str(twitter_stats)
glimpse(twitter_stats)

# Export to csv for analysis 
# x <- wine_data_Reduced %>% select(designation)
# write.table(x, file = "foo.csv", sep = ",", col.names = NA,
#             qmethod = "double")

# Add color
wine_data_with_color <- merge(wine_data, colors, by = "variety")

nrow(wine_data)
#[1] 129833
nrow(wine_data_with_color)
#[1] 126407
nrow(wine_data) - nrow(wine_data_with_color)
# 3426 records missing

# List of records i am not getting merged by ID
setdiff(wine_data$X, wine_data_with_color$X)

wine_data <- wine_data_with_color

wine_data <- as_tibble(wine_data)

wine_data_original <- wine_data

