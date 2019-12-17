# Load most commonly used libraries

list.of.packages <- c("tidyverse", "plotly", "here", "ggthemes", "stringr", "plyr", "stringi", "readxl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library("plyr")
library("tidyverse")
library("here")
library("ggthemes")
library("stringr")
library("stringi")
library("readxl")
library("ggExtra")

# ---- constants ----

VARIETY_PER_COLOR_LUMP <- 5
TASTER_NAME_LUMP <- 5
TASTER_TWITTER_LUMP <- 5
DESIGNATION_LUMP <- 10
COUNTRY_LUMP <- 10
VARIETY_LUMP <- 10

# ---- begin ----
# Step One: Load Data:
source("code/load.R")

# ---- clean ----
# Step Two: Clean Data:
source("code/clean.R")

# ---- feature generation ----
# Step Three: Feature Engeneering:
source("code/features.R")

summary(wine_data)

#Plots

ggplot(wine_data, aes(x = point_cat, y = price)) +
  geom_bar(color = "purple", stat = "identity") + ggtitle("Point_Cat and Price")

ggplot(wine_data, aes(x = price, y = country_lump, color = point_cat)) +
  geom_jitter() + ggtitle("Price and Country Colored by Point_Cat")

ggplot(wine_data, aes(taster_review_count, fill = taster_avg_points)) + geom_density(position = "stack", color = "purple") + ggtitle("Density Plot of Twitter Data")

wine_data %>% ggplot(aes(x = reorder(color, points), y = points, fill = color)) + geom_boxplot() + 
  xlab("Color") + theme_clean() + facet_wrap(~ taster_gender + title_has_accents) #+ theme(legend.position = "none") 

wine_data %>% ggplot(aes(x = reorder(color_lump, points), y = points, fill = color_lump)) + geom_boxplot() + 
  xlab("Color") + theme_clean() + facet_wrap(~ taster_gender + title_has_accents) + theme(legend.position = "none") 

ggplot(wine_data, aes(price, points, color = point_cat)) + geom_point() + theme_fivethirtyeight() + labs(title = "Score vs Price") 

ggplot(wine_data, aes(price, points, color = point_cat)) + geom_point() + theme_fivethirtyeight() + labs(title = "Score vs Price") #+ xlim(0, 100)


ggplot(wine_data, aes(price, points, color = point_cat)) + geom_jitter(alpha =
                                                                         1 / 10) + theme_fivethirtyeight() + labs(title = "Score vs Price") + xlim(0, 100)

ggplot(wine_data, aes(price, points, color = point_cat)) + geom_point() + theme_fivethirtyeight() + labs(title = "Score vs Price") + xlab("Points (0-100)") +
  ylab("Price ($)")
