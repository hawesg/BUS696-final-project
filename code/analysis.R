###############################################################################-
#                                                                              #
# Purpose:       This is the jumping off point for analysis. Individual model  #
#                s can be found in code/models/                                #
#                                                                              #
# Author:        Garrett H, Bart C, Ricky L                                    #
# Contact:       hawes102@mail.chapman.edu                                     #
# Client:        Jonathan Hersh                                                #
#                                                                              #
# Code created:  2019-11-22                                                    #
# Last updated:  2019-11-22                                                    #
# Source:        /Users/garretthawes/wine-project                              #
#                                                                              #
# Comment:       For subsiquent work to remain consistent testing and          #
#                training set can be loaded with:                              #
#                load(here::here("data","output","wine_train.RData"))          #
#                load(here::here("data","output","wine_test.RData"))           #
#                Also tosave resources while playing around testing set        #
#                can be limited for example:                                   #
#                data = wine_train %>% sample_n(10000)                         #
#                                                                              #
###############################################################################-

library("leaps")

# load(here::here("data", "output", "clean_wine.RData"))

load(here::here("data","output","clean_wine.RData"))

############################### Test/Train Setup ###############################

wine_data_non_bart <-
  subset(wine_data_clean, select=-c(title.n_words_per,
                        taster.avg_points_per,
                        taster.n_reviews_per,
                        taster.n_tweets_per,
                        taster.n_followers_per))
wine_data_bart <- wine_data_non_bart %>% mutate(title.n_words = wine_data_clean$title.n_words_per,
                                                 taster.avg_points = wine_data_clean$taster.avg_points_per,
                                                 taster.n_reviews = wine_data_clean$taster.n_reviews_per,
                                                 taster.n_tweets = wine_data_clean$taster.n_tweets_per,
                                                 taster.n_followers = wine_data_clean$taster.n_followers_per)
wine_data_clean <- wine_data_bart
set.seed(1861)
options(scipen = 50)
train_idx <-
  sample(1:nrow(wine_data_clean), size = floor(nrow(wine_data_clean) * .75))
wine_train <- wine_data_clean %>% slice(train_idx)
wine_test <- wine_data_clean %>% slice(-train_idx)

save(wine_train, file = here::here("data","output","wine_train.RData"))
save(wine_test, file = here::here("data","output","wine_test.RData"))
names(wine_test)

#################################### Models ####################################

# ---- Step ----
# Step One: Load Data:
source("code/models/step.R")

# ---- OLS ----
# Step One: Load Data:
source("code/models/ols.R")

# ---- E Net ----
# Step One: Load Data:
source("code/models/enet.R")

