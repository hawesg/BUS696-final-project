# Load most commonly used libraries


# need to add the following libraries:
#  library('plotROC')
#

list.of.packages <- c("tidyverse", "plotly", "here", "ggthemes", "stringr", "plyr", "stringi", "readxl","plotROC")
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
library("plotROC")

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


names(wine_data_clean)

set.seed(1861)
trainSize <- 0.85
train_idx <- sample(1:nrow(wine_data_clean), size = floor(nrow(wine_data_clean) * trainSize))

wine_data_clean_log_model <- wine_data_clean %>%
                                select (
                                  price,
                                  points,
                                  variety_lump,
                                  taster_avg_points,
                                  country_lump,
                                  province_lump
                                )

logitmodel_train <- wine_data_clean_log_model %>% slice(train_idx)
logitmodel_test <- wine_data_clean_log_model %>% slice(-train_idx)

# getting the ratio from train data... then we can see if same ratio can be used on the test data
median_price_to_points_ratio = median(logitmodel_train$points)/median(logitmodel_train$price)
median_price_to_points_ratio

logitmodel_train <- logitmodel_train %>% 
  mutate ( good_wine = factor(ifelse(
    points/price > median_price_to_points_ratio,"Yes","No")
    )
  ) %>% select (-price,-points)

#view(logitmodel_train)

logitmodel_test <- logitmodel_test %>% 
  mutate ( good_wine = factor(ifelse(
    points/price > median_price_to_points_ratio,"Yes","No")
  )
  ) %>% select (-price,-points)


# remove n/a values
logitmodel_train <- logitmodel_train[apply(is.na(logitmodel_train),1,sum)==0,]
logitmodel_test <- logitmodel_test[apply(is.na(logitmodel_test),1,sum)==0,]

logit_mod <- glm( good_wine ~ .,
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

TrainDF <- data.frame(default = c(preds_trainset$good_wine),
                      scores = c(preds_trainset$scores_logit1),
                      models = c(rep("Train Data Set",length(preds_trainset$scores_logit1))))

TestDF <- data.frame(default = c(preds_testset$good_wine),
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

