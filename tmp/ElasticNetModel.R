# Load most commonly used libraries

# need to include: caret + lattice + glmnet + glmnetUtils + coefplot


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

ElasticNetModel_train <- wine_data_clean_rm_model %>% slice(train_idx)
ElasticNetModel_test <- wine_data_clean_rm_model %>% slice(-train_idx)


# remove n/a values
ElasticNetModel_train <- ElasticNetModel_train[apply(is.na(ElasticNetModel_train),1,sum)==0,]
ElasticNetModel_test <- ElasticNetModel_test[apply(is.na(ElasticNetModel_test),1,sum)==0,]

#using length 100
alpha_list <- seq(0,1,len = 101)
alpha_list

#get the model
conflict_prefer("mutate", "dplyr")
enet_fit <- cva.glmnet(price ~ ., 
                       data = ElasticNetModel_train, 
                       alpha = alpha_list)

print(enet_fit)

### minlossplot
minlossplot(enet_fit)
plot(enet_fit)

#  from the above the optimal elasticnet model is at alpha 0.28, this is more towards ridge (alpha =0) and less towards lasso (alpha=1); but it kind of lands in the middle so it is the best of both worlds
# best value at alpha =0.28 where the cross validation loss is the lowest

plot(enet_fit$modlist[[28]])

# other candidates
plot(enet_fit$modlist[[1]])  ## alphas zero, ridge model
plot(enet_fit$modlist[[20]])
plot(enet_fit$modlist[[28]]) ## best
plot(enet_fit$modlist[[42]])
plot(enet_fit$modlist[[66]])
plot(enet_fit$modlist[[101]]) ## alphas zero, lasso  model


# coefficient matrix for the optimal elasticnet model using lambda.1se
coef(enet_fit, alpha = 0.28, 
     s = enet_fit$modlist[[28]]$lambda.1se)

