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


######################### Center and Scale predictors ##########################

wine_data_to_be_standardized <-
  wine_data_clean %>% 
  select(
    points,
    title.n_words,
    title.sentement,
    title.n_chars,
    taster.avg_points,
    taster.n_reviews,
    taster.n_tweets,
    taster.n_followers
  )
wine_data_not_to_be_standardized <-
  wine_data_clean %>% 
  select(
    price,
    points.category,
    country,
    province,
    winery,
    color,
    variety,
    variety_and_color,
    designation,
    title.has_accents,
    taster.name,
    taster.gender
  )

preprocessParams <-
  preProcess(wine_data_to_be_standardized[, 1:8], method = c("center", "scale"))
print(preprocessParams)

transformed <-
  predict(preprocessParams, wine_data_to_be_standardized[, 1:8])
summary(transformed)
head(transformed)
wine_data_standardized <- bind_cols(wine_data_not_to_be_standardized, transformed)
# str(wine_data_standardized)
# library("skimr")
# skim(wine_data_standardized)

############################### Test/Train Split ###############################

# Setup Test and Train set with CARET so they are proportianal
set.seed(1861)
options(scipen = 50)
TRAIN.PERCENT <- 0.75 
inTrainSetIndex <- createDataPartition(y = wine_data_standardized$price, p=TRAIN.PERCENT, list=FALSE, groups=5)
data.train   <- wine_data_standardized[ inTrainSetIndex, ]
data.test <- wine_data_standardized[-inTrainSetIndex, ]

# Save Test & Train

str(data.train)

# saveRDS(data.train, file = here::here("data","output","wine_train_100.rds"))
# saveRDS(data.test, file = here::here("data","output","wine_test_100.rds"))
# 
# data.train <- loadRDS(here::here("data","output","wine_train_100.rds"))
# saveRDS(data.test, file = here::here("data","output","wine_test_100.rds"))
      
# ---- OLS Regression and Stepwise Selection ----
source("code/models/1.ols.and.step.R")

# ---- Step Slection ----
# source("code/models/2.step.R")

# ---- Elastic Net ----
source("code/models/3.enet.R")

# ---- Bagging and Bootstrapping ----
source("code/models/4.bootstraping.R")

# ---- Bagging and Bootstrapping ----
source("code/models/5.rforest.R")

# ---- Logistic Regression ----
source("code/models/6.logit.R")