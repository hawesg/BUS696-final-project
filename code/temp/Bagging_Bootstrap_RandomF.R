# Load most commonly used libraries

# need to include: ElemStatLearn + partykit + randomForest + randomForestExplainer


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

library('ElemStatLearn')
library('partykit')
library('magrittr')

library('randomForest')
library('randomForestExplainer')

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
trainSize <- 0.10
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

Model_train <- wine_data_clean_rm_model %>% slice(train_idx)
Model_test <- wine_data_clean_rm_model %>% slice(-train_idx)


# remove n/a values
Model_train <- Model_train[apply(is.na(Model_train),1,sum)==0,]
Model_test <- Model_test[apply(is.na(Model_test),1,sum)==0,]

# store rownames as columns
Model_train_preds <- Model_train %>% rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))

# bagging - bootstrapp aggregation
B <- 100      # number of bootstrap samples
num_b <- 250  # sample size of each bootstrap
boot_mods <- list() # store our bagging models
for(i in 1:B){
  boot_idx <- sample(1:nrow(Model_train), 
                     size = num_b,
                     replace = FALSE)
  # fit a tree on each bootstrap sample
  boot_tree <- ctree(price ~ ., 
                     data = Model_train %>% 
                       slice(boot_idx)) 
  # store bootstraped model
  boot_mods[[i]] <- boot_tree
  # generate predictions for that bootstrap model
  preds_boot <- data.frame(
    preds_boot = predict(boot_tree),
    rowname = boot_idx 
  )  
  
  # rename prediction to indicate which boot iteration it came from
  names(preds_boot)[1] <- paste("preds_boot",i,sep = "")
  
  # merge predictions to Model_train dataset
  Model_train_preds <- left_join(x = Model_train_preds, y = preds_boot,
                              by = "rowname")
}


## examine some of the individual models
plot(boot_mods[[1]])

plot(boot_mods[[10]])

plot(boot_mods[[20]])


# must convert factor into numeric, note that class "0" = 1, 
# and class "1" = 2, so we need to subtract 1 from every column
Model_train_preds %<>% mutate_if(is.factor, as.numeric) %>% 
  mutate_all(function(x){x - 1})

# calculate mean over all the bootstrap predictions
Model_train_preds %<>% mutate(preds_bag = 
                             select(., preds_boot1:preds_boot100) %>% 
                             rowMeans(na.rm = TRUE))

# congratulations! You have bagged your first model!
ggplot(Model_train_preds, aes(x = preds_bag)) + geom_histogram()



#---------------------------------------------------------------
# Random Forests
#---------------------------------------------------------------

rf_fit <- randomForest(price ~ . ,
                       data = Model_train,
                       type = classification,
                       mtry = 3,
                       ntree = 1000,
                       importance = TRUE,
                       localImp = TRUE)

rf_fit

#---------------------------------------------------------------
# Explaining Random Forests
#---------------------------------------------------------------
# install.packages('randomForestExplainer')
library(randomForestExplainer)
# plot min
plot_min_depth_distribution(rf_fit)

plot_multi_way_importance(rf_fit)

plot_multi_way_importance(rf_fit, x_measure = "mse_increase",
                          y_measure = "node_purity_increase")
# 
plot_predict_interaction(rf_fit, Model_train, "taster_n_tweets_per", "points")

# explanation file 
explain_forest(rf_fit, interactions = TRUE, data = Model_train)

# relations between measure of importance
plot_importance_ggpairs(rf_fit)

#---------------------------------------------------------------
# Tuning Random Forests
#---------------------------------------------------------------
rf_mods <- list()
oob_err <- NULL
test_err <- NULL
for(mtry in 1:9){
  rf_fit <- randomForest(price ~ ., 
                         data = Model_train,
                         mtry = mtry,
                         ntree = 500)
  oob_err[mtry] <- rf_fit$err.rate[500]
  
  cat(mtry," ")
}

results_DF <- data.frame(mtry = 1:9,oob_err)

ggplot(results_DF, aes(x = mtry, y = oob_err)) + geom_point()

