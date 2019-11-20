# Load most commonly used libraries

# need to include: caret + lattice + glmnet + glmnetUtils + coefplot


list.of.packages <-
  c("tidyverse",
    "plotly",
    "here",
    "ggthemes",
    "stringr",
    "plyr",
    "stringi",
    "readxl",
    ".")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

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

library('lattice')
library("caret")

library("glmnet")
library("glmnetUtils")
library('coefplot')

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

wine_data_clean_rm_model <- wine_data_clean %>%
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
    taster_avg_points_per,
    # no added value
    #taster_review_count,
    taster_review_count_per,
    # no added value
    #taster_n_tweets,
    taster_n_tweets_per,
    designation_lump,
    color_lump,
    #title_word_count,
    # title_word_count_per, # no added value
    #title_sentement # no added value
  )

RidgeModel_train <- wine_data_clean_rm_model %>% slice(train_idx)
RidgeModel_test <- wine_data_clean_rm_model %>% slice(-train_idx)


# remove n/a values
RidgeModel_train <-
  RidgeModel_train[apply(is.na(RidgeModel_train), 1, sum) == 0, ]
RidgeModel_test <-
  RidgeModel_test[apply(is.na(RidgeModel_test), 1, sum) == 0, ]



Ridge_mod <- glmnetUtils::cv.glmnet(price ~ .,
                                    data = RidgeModel_train,
                                    alpha = 0)

#lambdas
sprintf("Lambda Min = %f", Ridge_mod$lambda.min)

sprintf("Lambda 1se = %f", Ridge_mod$lambda.1se)

# min lambda coeffs
coef_mat <- data.frame (
  varname = rownames(coef(Ridge_mod)) %>% data.frame(),
  ridgeLambdaMin = as.matrix(coef(Ridge_mod, s = Ridge_mod$lambda.min) %>%  round(8)),
  ridgeLambda1se = as.matrix(coef(Ridge_mod, s = Ridge_mod$lambda.1se) %>%  round(8))
) %>%
  rename(varname = 1,
         ridgeLamdaMin = 2,
         ridgeLamda1se = 3) %>%
  remove_rownames ()

coef_mat

# explore how coefficients change as we change lambda
coefpath(Ridge_mod)

# generating predictions
### ======================================================



### Ridge in sample (train)
dataframeToEval2 <- data.frame (
  actual = RidgeModel_train$price,
  pred   = predict(Ridge_mod, lamda = Ridge_mod$lambda.min, newdata = RidgeModel_train)
) %>% rename(actual = 1, pred = 2) %>%  remove_rownames()

postResample(dataframeToEval2$pred, dataframeToEval2$actual)


### Ridge out of sample (test)
dataframeToEval2 <- data.frame (
  actual = RidgeModel_test$price,
  pred   = predict(Ridge_mod, lamda = Ridge_mod$lambda.min, newdata = RidgeModel_test)
) %>% rename(actual = 1, pred = 2) %>%  remove_rownames()

postResample(dataframeToEval2$pred, dataframeToEval2$actual)
