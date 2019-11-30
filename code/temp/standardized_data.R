library(caret)
# dput(names(wine_data_clean))
wine_data_to_be_standardized <-
  wine_data_clean %>% select(
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
  wine_data_clean %>% select(
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

# wine_data_standardized <-
#   wine_data_clean %>% select(
#     price,
#     title.has_accents,``
#     points.category,
#     country,
#     province,
#     winery,
#     color,
#     variety,
#     variety_and_color,
#     designation,
#     title.has_accents,
#     taster.name,
#     taster.gender,
#     points,
#     title.n_words,
#     title.sentement,
#     title.n_chars,
#     taster.avg_points,
#     taster.n_reviews,
#     taster.n_tweets,
#     taster.n_followers
#   )

summary(wine_data_to_be_standardized[, 1:8])
preprocessParams <-
  preProcess(wine_data_to_be_standardized[, 1:8], method = c("center", "scale"))
print(preprocessParams)
transformed <-
  predict(preprocessParams, wine_data_to_be_standardized[, 1:8])
summary(transformed)
head(transformed)
wine_data_standardized <- bind_cols(wine_data_not_to_be_standardized, transformed)
str(wine_data_standardized)
library("skimr")
skim(wine_data_standardized)

library(glmnet)
library(glmnetUtils)

alpha_list <- seq(0, 1, len = 101)
alpha_list
set.seed(1861)
options(scipen = 50)
TRAIN.PERCENT <- 0.75 
inTrainSetIndex <- createDataPartition(y = wine_data_standardized$price, p=TRAIN.PERCENT, list=FALSE, groups=5)
data.train   <- wine_data_standardized[ inTrainSetIndex, ]
data.test <- wine_data_standardized[-inTrainSetIndex, ]

enet_fit <- cva.glmnet(
  -1 * price ^ (-.3) ~ .,
  data = data.train %>% select(
    -variety_and_color
  )%>%sample_n(100),
  trace.it=1
  ,
  alpha = alpha_list
)
