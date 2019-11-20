# Load most commonly used libraries


# need to add the following libraries:
#  library('plotROC')
#

list.of.packages <- c("tidyverse", "plotly", "here", "ggthemes", "stringr", "plyr", "stringi", "readxl",".")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

#install.packages("devtools")
#install.packages("conflicted")

library("plyr")
library("conflicted")
#library("ggmap")

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
library("plotROC")
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

wine_data_clean_log_model <- wine_data_clean %>%
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
    title_word_count_per,
    title_sentement
  ) 


logitmodel_train <- wine_data_clean_log_model %>% slice(train_idx)
logitmodel_test <- wine_data_clean_log_model %>% slice(-train_idx)

# getting the ratio from train data... then we can see if same ratio can be used on the test data
min_points_model_value = min(logitmodel_train$points) 
min_points_model_value
min_price_model_value = 3.5
min_price_model_value
median_price_to_points_ratio = 
  (median(logitmodel_train$points)-min_points_model_value)/(median(log(ifelse(min_price_model_value > logitmodel_train$price,min_price_model_value + 0.1,logitmodel_train$price))) - log(min_price_model_value))
median_price_to_points_ratio


logitmodel_train <- logitmodel_train %>% 
  dplyr::mutate ( well_priced = factor(ifelse(
    (points-min_points_model_value)/(log(ifelse(min_price_model_value > price,min_price_model_value + 0.1,price))-log(min_price_model_value))  > median_price_to_points_ratio,"Yes","No")
  )
  ) 


#view(logitmodel_train) log
ggplot(logitmodel_train , aes(x = price, y = points, color = well_priced)) +
  geom_jitter() + ggtitle("Price and Points Colored by Well Priced") + 
  theme(legend.position = "top") + labs(fill = "Well Priced") 

#view(logitmodel_train) log
ggplot(logitmodel_train , aes(y = price, x = points, color = well_priced)) +
  geom_jitter() + ggtitle("Price and Points Colored by Well Priced") + 
  theme(legend.position = "top") + labs(fill = "Well Priced") + scale_y_log10()





logitmodel_test <- logitmodel_test %>% 
  dplyr::mutate ( well_priced = factor(ifelse(
    (points-min_points_model_value)/(log(ifelse(min_price_model_value > price,min_price_model_value + 0.1,price))-log(min_price_model_value))  > median_price_to_points_ratio,"Yes","No")
  )
  )

# good wine already has price and points, so we don't want to model as part of it
logitmodel_train <- logitmodel_train %>% select (-price,-points)
logitmodel_test <- logitmodel_test  %>% select (-price,-points)


# remove n/a values
logitmodel_train <- logitmodel_train[apply(is.na(logitmodel_train),1,sum)==0,]
logitmodel_test <- logitmodel_test[apply(is.na(logitmodel_test),1,sum)==0,]

logit_mod <- glm( well_priced ~ .,
                  data = logitmodel_train,
                  family = binomial) #our varaible can be 0 or 1, a binomial
summary(logit_mod)


### predictions
preds_trainset<- data.frame (
  scores_logit1 = predict(logit_mod,newdata=logitmodel_train,type="response"),
  logitmodel_train
) 

## test set
preds_testset<- data.frame (
  scores_logit1 = predict(logit_mod,newdata=logitmodel_test,type="response"),
  logitmodel_test
) 
summary (preds_testset)


#### ROC Curve
#head(preds_testset)

TrainDF <- data.frame(default = c(preds_trainset$well_priced),
                      scores = c(preds_trainset$scores_logit1),
                      models = c(rep("Train Data Set",length(preds_trainset$scores_logit1))))

TestDF <- data.frame(default = c(preds_testset$well_priced),
                     scores = c(preds_testset$scores_logit1),
                     models = c(rep("Test Data Set",length(preds_testset$scores_logit1))))


### ROC Curve train
TrainROC <- ggplot(TrainDF, aes(m = scores, d = default, color = models)) + 
  geom_roc(show.legend = TRUE, labelsize = 3.5, cutoffs.at = c(.99,.9,.8,.7,.5,.3,.1,0))
TrainROC <- TrainROC + style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  theme(legend.justification = c(1, 0), 
        legend.position = c(1, 0),
        legend.box.margin=margin(c(50,50,50,50)))
plot(TrainROC)

### ROC Curve test
TestROC <- ggplot(TestDF, aes(m = scores, d = default, color = models)) + 
  geom_roc(show.legend = TRUE, labelsize = 3.5, cutoffs.at = c(.99,.9,.8,.7,.5,.3,.1,0))
TestROC <- TestROC + style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  theme(legend.justification = c(1, 0), 
        legend.position = c(1, 0),
        legend.box.margin=margin(c(50,50,50,50)))
plot(TestROC)

### Area under the curve
AUC_results <- data.frame (
  TrainAUC = calc_auc(TrainROC),
  TestAUC =calc_auc(TestROC)
)
AUC_results
