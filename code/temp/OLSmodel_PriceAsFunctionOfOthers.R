# Load most commonly used libraries

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

wine_data_clean_ols_model <- wine_data_clean %>%
  select (
    price,
    points,
    # point_cat,   
    variety_lump,  
    country_lump,
    province_lump,   
    taster_name_lump,
    # taster_twitter_lump,  #not helpful
    taster_gender,
    taster_avg_points,
    taster_avg_points_per,   # no added value
    taster_review_count,
    taster_review_count_per,  # no added value
    taster_n_tweets,
    taster_n_tweets_per,
    designation_lump,
    color_lump,
    #title_word_count,
    title_word_count_per, # no added value
    title_sentement # no added value
  ) 


OLSmodel_train <- wine_data_clean_ols_model %>% slice(train_idx)
OLSmodel_test <- wine_data_clean_ols_model %>% slice(-train_idx)


# remove n/a values
OLSmodel_train <- OLSmodel_train[apply(is.na(OLSmodel_train),1,sum)==0,]
OLSmodel_test <- OLSmodel_test[apply(is.na(OLSmodel_test),1,sum)==0,]

OLS_mod <- lm( price ~ .,
                  data = OLSmodel_train)

str(OLSmodel_train %>% select_if(is.factor))
summary(OLS_mod)


### predictions
preds_trainset<- data.frame (
  scores_OLS1 = predict(OLS_mod,newdata=OLSmodel_train),
  OLSmodel_train
) 

## test set
preds_testset<- data.frame (
  scores_OLS1 = predict(OLS_mod,newdata=OLSmodel_test),
  OLSmodel_test
) 


summary (preds_testset)


# put model preds and true in a data frame
preds_DF_train <- data.frame(
  price_pred = predict(OLS_mod,newdata=OLSmodel_train),
  resids = OLSmodel_train$price - predict(OLS_mod, newdata = OLSmodel_train),
  resids_log = log(abs(OLSmodel_train$price - predict(OLS_mod, newdata = OLSmodel_train))),
  OLSmodel_train 
)

# training data set plot of  residuals against predicted
ggplot(preds_DF_train, aes(x = price_pred, y = resids, color=color_lump)) + geom_point(alpha=0.05) + stat_smooth(method = "lm")  +  ylim(-100,200)

# training data set plot of residuals against predicted
ggplot(preds_DF_train, aes(x = price_pred, y = resids_log, color=color_lump)) + geom_point(alpha=0.3) + stat_smooth(method = "lm") +  ylim(-8,8)

## --

# put model preds and true in a data frame
preds_DF_test <- data.frame(
  price_pred = predict(OLS_mod,newdata=OLSmodel_test),
  resids = OLSmodel_test$price - predict(OLS_mod, newdata = OLSmodel_test),
  resids_log = log(abs(OLSmodel_test$price - predict(OLS_mod, newdata = OLSmodel_test))),
  OLSmodel_test 
)

# test data set plot of  residuals against predicted
ggplot(preds_DF_test, aes(x = price_pred, y = resids, color=color_lump)) + geom_point(alpha=0.05) + stat_smooth(method = "lm")  +  ylim(-100,200)

# test data set plot of log residuals against predicted
ggplot(preds_DF_test, aes(x = price_pred, y = resids_log, color=color_lump)) + geom_point(alpha=0.3) + stat_smooth(method = "lm")  +  ylim(-8,8)

### Residuals summary
#Residuals_Summary <- data.frame (
  Train_summary = data.frame (
      Median = median(preds_DF_train$resids),
      Average = mean(preds_DF_train$resids),
      Min = min(preds_DF_train$resids),
      Max = max(preds_DF_train$resids)
  )#,
  Test_summary = data.frame (
      Median = median(preds_DF_test$resids),
      Average = mean(preds_DF_test$resids),
      Min = min(preds_DF_test$resids),
      Max = max(preds_DF_test$resids)
  )
#)

Train_summary
Test_summary

