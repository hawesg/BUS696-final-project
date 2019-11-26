# new logit


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


conflict_prefer("mutate", "dplyr")
conflict_prefer("margin","ggplot2")


# Load data 100 (could use other) -----------------------------------------

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

##
# Load Model_train / Model_test ----
load(here::here("data","output","limited_factors","wine_train.RData"))
load(here::here("data","output","limited_factors","wine_test.RData"))
names(wine_train)
names(wine_test)

wine_train <- wine_train[apply(is.na(wine_train),1,sum)==0,]
wine_test <- wine_test[apply(is.na(wine_test),1,sum)==0,]


# getting the ratio from train data ---- 
# then we can see if same ratio can be used on the test data
min_points_model_value = min(wine_train$points) 
min_points_model_value
min_price_model_value = 2.5
min_price_model_value
median_price_to_points_ratio = 
  (median(wine_train$points)-min_points_model_value)/(median(log(ifelse(min_price_model_value > wine_train$price,min_price_model_value + 0.1,wine_train$price))) - log(min_price_model_value))
median_price_to_points_ratio


# Compute well_priced for train ----
wine_train <- wine_train %>% 
  dplyr::mutate ( well_priced = factor(ifelse(
    (points-min_points_model_value)/(log(ifelse(min_price_model_value > price,min_price_model_value + 0.1,price))-log(min_price_model_value))  > median_price_to_points_ratio,"Yes","No")
  )
  ) 


#view(wine_train) regular scale ----
ggplot(wine_train , aes(x = price, y = points, color = well_priced)) +
  geom_jitter() + ggtitle("Price and Points Colored by Well Priced") + 
  theme(legend.position = "top") + labs(fill = "Well Priced") 

#view(wine_train) log scale ----
ggplot(wine_train , aes(y = price, x = points, color = well_priced)) +
  geom_jitter() + ggtitle("Price and Points Colored by Well Priced") + 
  theme(legend.position = "top") + labs(fill = "Well Priced") + scale_y_log10()

# Compute well_priced for test ----
wine_test <- wine_test %>% 
  dplyr::mutate ( well_priced = factor(ifelse(
    (points-min_points_model_value)/(log(ifelse(min_price_model_value > price,min_price_model_value + 0.1,price))-log(min_price_model_value))  > median_price_to_points_ratio,"Yes","No")
  )
  )

# good wine already has price and points, so we don't want to model as part of it ----
wine_train <- wine_train %>% select (-price,-points)
wine_test <- wine_test  %>% select (-price,-points)


# remove n/a values ----
wine_train <- wine_train[apply(is.na(wine_train),1,sum)==0,]
wine_test <- wine_test[apply(is.na(wine_test),1,sum)==0,]

names(wine_train)

# let's create the model ----
logit_mod <- glm( well_priced ~ .,
                  data = wine_train %>%   select (
                    well_priced,
                    points.category,   
                    variety,  
                    country,
                    province,   
                    taster.name,
                    taster.gender,
                    taster.avg_points,
                    designation,
                    color,
                    title.n_words,
                    title.sentement
                  ) ,
                  family = binomial) #our varaible can be 0 or 1, a binomial
# summary of model ----
summary(logit_mod)


### predictions -----
preds_trainset<- data.frame (
  scores_logit1 = predict(logit_mod,newdata=wine_train,type="response"),
  wine_train
) 

## test set
preds_testset<- data.frame (
  scores_logit1 = predict(logit_mod,newdata=wine_test,type="response"),
  wine_test
) 
summary (preds_testset)


#### ROC Curve ----
#head(preds_testset)

TrainDF <- data.frame(default = c(preds_trainset$well_priced),
                      scores = c(preds_trainset$scores_logit1),
                      models = c(rep("Train Data Set",length(preds_trainset$scores_logit1))))

TestDF <- data.frame(default = c(preds_testset$well_priced),
                     scores = c(preds_testset$scores_logit1),
                     models = c(rep("Test Data Set",length(preds_testset$scores_logit1))))


### ROC Curve train -----
TrainROC <- ggplot(TrainDF, aes(m = scores, d = default, color = models)) + 
  geom_roc(show.legend = TRUE, labelsize = 3.5, cutoffs.at = c(.99,.9,.8,.7,.5,.3,.1,0))
TrainROC <- TrainROC + style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  theme(legend.justification = c(1, 0), 
        legend.position = c(1, 0),
        legend.box.margin=margin(c(50,50,50,50)))
plot(TrainROC)

### ROC Curve test ----
TestROC <- ggplot(TestDF, aes(m = scores, d = default, color = models)) + 
  geom_roc(show.legend = TRUE, labelsize = 3.5, cutoffs.at = c(.99,.9,.8,.7,.5,.3,.1,0))
TestROC <- TestROC + style_roc(theme = theme_grey) +
  theme(axis.text = element_text(colour = "blue")) +
  theme(legend.justification = c(1, 0), 
        legend.position = c(1, 0),
        legend.box.margin=margin(c(50,50,50,50)))
plot(TestROC)

### Area under the curve ----
AUC_results <- data.frame (
  TrainAUC = calc_auc(TrainROC),
  TestAUC =calc_auc(TestROC)
)
AUC_results
