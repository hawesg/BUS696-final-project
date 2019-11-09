################### LOAD DATA ###################
wine_data <-
  read.csv(here("data", "input", "winemag-data-130k-v2.csv"))
colors <- read.csv(here("data", "input", "wine-colors.csv"))
dim(wine_data)

################### Clean ###################

wine_data <-
  wine_data %>% mutate (country = as.character(country), variety = as.character(variety)) %>%
  filter (country != "England" &
            country != "US-France" &
            country != "" &
            variety != "") %>% drop_na(price)

# TODO

################### Feature Generation ##################

# TODO Decide on factors for price groups budget, ... premium, ultra premium? then use cut to devide them into buckets.

# Turn score into categories per https://www.winespectator.com/articles/scoring-scale
wine_data <-
  wine_data  %>% mutate (point_cat = cut(
    wine_data$points,
    breaks = c(0, 74, 79, 84, 89, 94, 100),
    labels = c(
      "Not Recomended",
      "Mediocre",
      "Good",
      "Very good",
      "Outstanding",
      "Classic"
    )
  ))



# Add a column to indicate wheather the wine includes 
wine_data <-
  wine_data  %>% mutate (vintage = str_extract(title, "(19\\d{2}|20\\d{2})") )

                           
                           
                           #sub("\\d", title) )


# Add color
wine_data_with_color <- merge(wine_data, colors, by = "variety")

nrow(wine_data)
#[1] 129833
nrow(wine_data_with_color)
#[1] 126407
nrow(wine_data) - nrow(wine_data_with_color)
# 3426 records missing

options(scipen = 99999)
#seq(0, 3000, by = 25)
stats <- wine_data %>% group_by(pr=cut(price, breaks= c(0, 10, 25, 50, 100, 200, 500, 1000, 3500), dig.lab = 5  )) %>% summarize(count = n(), min = min(points), 
                                                              max = max(points), avg = mean(points), sd = sd(points))
stats


# List of
setdiff(wine_data$X, wine_data_with_color$X)



country_levels <- factor(wine_data$country)

levels(country_levels)
