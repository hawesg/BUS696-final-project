# Load most commonly used libraries

# to do: install 'leaps'

list.of.packages <- c("tidyverse", "plotly", "here", "ggthemes", "stringr", "plyr", "stringi", "readxl",".")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

#install.packages("devtools")
#install.packages("conflicted")

library("conflicted")
#library("ggmap")
#library("plyr")
library("tidyverse")
library("here")
library("ggthemes")
library("stringr")
library("stringi")
library("readxl")
library("ggExtra")
library("PerformanceAnalytics")
#library("GGally")
#library("qwraps2")
library('sentimentr')
library('leaps')


# ---- constants ----


# Set fct_lump size for the various times that fct_lump is used.
# FCT_LUMPS <-
#   c(
#     variery_color = 5,
#     taster_name = 1,
#     taster_twitter = 5,
#     designation = 10,
#     country = 10,
#     variety = 10
#   )
# test4<-fct_lump(test, n=FCT_LUMPS["taster_name"])

VARIETY_PER_COLOR_LUMP <- 5
TASTER_NAME_LUMP <- 5
TASTER_TWITTER_LUMP <- 5
DESIGNATION_LUMP <- 10
COUNTRY_LUMP <- 10
VARIETY_LUMP <- 10


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
load(here::here("data","output","clean_wine.RData"))

#wine_data_clean <- wine_data

names(wine_data_clean)


set.seed(1861)
trainSize <- 0.30
train_idx <- sample(1:nrow(wine_data_clean), size = floor(nrow(wine_data_clean) * trainSize))

wine_data_clean_fwd_model <- wine_data_clean %>%
  select (
    price,
    points,
    point_cat,   
    variety_lump,  
    country_lump,
    province_lump,   
    taster_name_lump,
    # taster_twitter_lump,  #not helpful
    taster_gender,
    #taster_avg_points,
    taster_avg_points_per,   # no added value
    #taster_review_count,
    taster_review_count_per,  # no added value
    #taster_n_tweets,
    taster_n_tweets_per,
    designation_lump,
    color_lump,
    #title_word_count,
    # title_word_count_per, # no added value
    #title_sentement # no added value
  ) 



fwd_fit <- 
  regsubsets(price ~ ., 
             data = wine_data_clean_fwd_model,
             nvmax = 5,
             method = "forward")

summary(fwd_fit)

plot(fwd_fit, scale = "adjr2")
