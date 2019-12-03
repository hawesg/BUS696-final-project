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
library("caret")

names(wine_data_clean)

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


# Setup Test and Train set with CARET so they are proportianal
set.seed(1861)
options(scipen = 50)
TRAIN.PERCENT <- 0.75 
inTrainSetIndex <- createDataPartition(y = wine_data_standardized$price, p=TRAIN.PERCENT, list=FALSE, groups=5)
data.train   <- wine_data_standardized[ inTrainSetIndex, ]
data.test <- wine_data_standardized[-inTrainSetIndex, ]

# Save Test & Train

str(data.train)

saveRDS(data.train, file = here::here("data","output","wine_train_100.rds"))
saveRDS(data.test, file = here::here("data","output","wine_test_100.rds"))

#################################### HELPER FUNCTIONS ####################################

.tuky <- function(p){
  p <- p ^ (-.3)
  return(-p)
}
 

.model_summary <- function(p, a, m){
  
  summary_df <- data.frame(pred = p,
                        actual = a,
                        resid = a-p)
  
  ggplot(summary_df, aes(x = actual, y = pred)) + 
    geom_point(color = "purple") +
    geom_abline(color = "red", linetype = "dashed") +
    ggtitle(paste(m,"Predicted vs Actuals")) 
  
  ggplot(summary_df, aes(x = pred, y = resid)) + 
    geom_point(color = "purple", alpha = 1 / 100) + 
    ggtitle(paste(m,"Residuals vs Predicted")) + 
    geom_smooth()
}

# .model_summary(preds, actuals, model_name)

#################################### Models ####################################

# # ---- OLS Regression ----
# source("code/models/1.ols.R")

# # ---- Step Slection ----
# source("code/models/2.step.R")

# ---- Elastic Net ----
source("code/models/3.enet.R")

# ---- Bagging and Bootstrapping ----
source("code/models/4.bootstraping.R")

# ---- Bagging and Bootstrapping ----
source("code/models/5.rforest.R")

# ---- Logistic Regression ----
source("code/models/6.logit.R")