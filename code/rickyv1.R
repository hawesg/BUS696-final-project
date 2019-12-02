#Source Libraries----

library('partykit')
library('rpart')
library('rpart.plot')
library('leaps')
library('tidyverse')
library('caret')
library('rcompanion')

#Import Data----

load(here::here("data", "output", "limited_factors", "wine_train.RData"))
load(here::here("data", "output", "limited_factors", "wine_test.RData"))

#Standardize----
wine_data_to_be_standardized <-
  wine_train %>% select(
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
  wine_train %>% select(
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
wine_data_standardized <-
  bind_cols(wine_data_not_to_be_standardized, transformed)
str(wine_data_standardized)
library("skimr")
skim(wine_data_standardized)

wine_train <- wine_data_standardized


#OlS Model (No Log Price) ----

ols_no_log <-
  lm(
    price ~
      country +
      variety +
      points +
      province +
      winery +
      color +
      designation +
      title.n_words +
      title.sentement +
      title.has_accents +
      taster.name +
      taster.gender,
    data = wine_train,
  )

summary(ols_no_log)
summary(ols_no_log)$r.squared

#Predictions-No Log Model ----

preds_no_log <- predict(ols_no_log)
preds_no_log

mod10_df <- data.frame(pred = preds_no_log,
                       actual = wine_train$price)

##Plot of Actual Vs Preds

ggplot(mod10_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

mod10_df <- data.frame(pred = preds_no_log,
                       actual = wine_train$price,
                       resids = ols_no_log$residuals)


##Plots of Preds Vs Resids

ggplot(mod10_df, aes(x = pred, y = resids)) + geom_point(color = "purple", alpha = 1 /
                                                           100) + ggtitle("OLS_NO_LOG RESIDS VS PREDS") + geom_smooth() + theme_bw()


#RMSE-OLS No Log

RMSE(mod10_df$pred, wine_train$price)

#MAE OLS No Log

MAE(mod10_df$pred, wine_train$price)


#Forward Fit Model (log price) ----

fwd_fit <-
  regsubsets(
    log(price) ~
      country +
      variety +
      points +
      province +
      winery +
      color +
      designation +
      title.n_words +
      title.sentement +
      title.has_accents +
      taster.name +
      taster.gender,
    data = wine_train,
    method = "forward",
    nvmax = 10
  )

summary(fwd_fit)

plot(fwd_fit, scale = "adjr2", main = "FORWARD SELECTION PROCEDURE")
coef(fwd_fit, 10)

##OLS Model Based on Fwd Fit

ols_from_fwd_fit <-
  lm(log(price) ~ province + color + points + taster.name + taster.gender + designation,
     data = wine_train)

summary(ols_from_fwd_fit)
summary(ols_from_fwd_fit)$r.squared

#Predictions-Forward Fit Model ----

preds_fwd_fit <- predict(ols_from_fwd_fit)
preds_fwd_fit

mod1_df <- data.frame(pred = preds_fwd_fit,
                      actual = wine_train$price)

##Plot of Actual Vs Preds

ggplot(mod1_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

mod1_df <- data.frame(pred = preds_fwd_fit,
                      actual = wine_train$price,
                      resids = ols_from_fwd_fit$residuals)


##Plots of Preds Vs Resids

ggplot(mod1_df, aes(x = pred, y = resids)) + geom_point(color = "purple", alpha = 1 /
                                                          100) + ggtitle("FWD_FIT MODEL RESIDS VS PREDS") + geom_smooth() + theme_bw()


##RMSE

RMSE(mod1_df$pred, wine_train$price)

##MAE

MAE(mod1_df$pred, wine_train$price)

#Backward Fit Model (log price) ----

bkwd_fit <-
  regsubsets(
    log(price) ~
      country +
      variety +
      points +
      province +
      winery +
      color +
      designation +
      title.n_words +
      title.sentement +
      title.has_accents +
      taster.name +
      taster.gender,
    data = wine_train,
    method = "backward",
    nvmax = 10
  )

summary(bkwd_fit)

plot(bkwd_fit, scale = "adjr2", main = "BACKWARD SELECTION PROCEDURE")
coef(bkwd_fit, 10)

##OLS Model Based on Bkwd

ols_from_bkwd_fit <-
  lm(log(price) ~ variety + province + points + taster.name + designation + taster.gender,
     data = wine_train)

summary(ols_from_bkwd_fit)
summary(ols_from_bkwd_fit)$r.squared

#Predictions-Backwards Fit Model----

preds_bkwd_fit <- predict(ols_from_bkwd_fit)
preds_bkwd_fit

mod3_df <- data.frame(pred = preds_bkwd_fit,
                      actual = wine_train$price)

##Plot of Actual Vs Preds

ggplot(mod3_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

##New Dataframe W/Resids Not Changed

mod3_df <- data.frame(pred = preds_bkwd_fit,
                      actual = wine_train$price,
                      resids = ols_from_bkwd_fit$residuals)


##Plot of Preds Vs Resids

ggplot(mod3_df, aes(x = pred, y = resids)) +
  geom_point(color = "purple", alpha = 1 / 100) + ggtitle("BKWD FIT MODEL RESIDS VS PREDS") + geom_smooth() + theme_bw()


##RMSE

RMSE(mod3_df$pred, wine_train$price)

##MAE

MAE(mod3_df$pred, wine_train$price)

#Tukey Experimentation----

sample <- sample(wine_train$price, size = 5000)
Tukey <- transformTukey(sample)
plotNormalHistogram(Tukey)

#Forward Fit Model-W/Tukey ----

fwd_fit_tukey <-
  regsubsets(
    -1 * price ^ (-.3) ~
      country +
      variety +
      points +
      province +
      winery +
      color +
      designation +
      title.n_words +
      title.sentement +
      title.has_accents +
      taster.name +
      taster.gender,
    data = wine_train,
    method = "forward",
    nvmax = 10
  )

summary(fwd_fit_tukey)

plot(fwd_fit_tukey, scale = "adjr2", main = "Forward Fit Model")
coef(fwd_fit_tukey, 10)

ols_from_fwd_fit_tukey <-
  lm(
    -1 * price ^ (-.3) ~ country + color + points + province + taster.gender + taster.name + designation,
    data = wine_train
  )

summary(ols_from_fwd_fit_tukey)
summary(ols_from_fwd_fit_tukey)$r.squared

#Predictions Fwd Fit Tukey----
##Note: Made Predict Function Negative to Generate Positive Predictions

preds_fwd_fit_tukey <- -predict(ols_from_fwd_fit_tukey)
preds_fwd_fit_tukey

mod5_df <- data.frame(pred = preds_fwd_fit_tukey,
                      actual = wine_train$price)

##Plot of Preds Vs Actual

ggplot(mod5_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

mod5_df <- data.frame(pred = preds_fwd_fit_tukey,
                      actual = wine_train$price,
                      resids = ols_from_fwd_fit_tukey$residuals)


##Plot of Resids vs Preds

ggplot(mod5_df, aes(x = pred, y = resids)) +
  geom_point(color = "purple", alpha = 1 / 100) + ggtitle("FWD FIT TUKEY RESIDS VS PREDS") + geom_smooth() + theme_bw()


##RMSE

RMSE(mod5_df$pred, wine_train$price)

##MAE

MAE(mod5_df$pred, wine_train$price)

#Backward Fit Model----

bkwd_fit_tukey <-
  regsubsets(
    -1 * price ^ (-.3) ~
      country +
      variety +
      points +
      province +
      winery +
      color +
      designation +
      title.n_words +
      title.sentement +
      title.has_accents +
      taster.name +
      taster.gender,
    data = wine_train,
    method = "backward",
    nvmax = 10
  )

summary(bkwd_fit_tukey)

plot(bkwd_fit_tukey, scale = "adjr2", main = "Backward Fit Model")
coef(bkwd_fit_tukey, 10)

ols_from_bkwd_fit_tukey <-
  lm(
    -1 * price ^ (-.3) ~ country + variety + points + province + taster.gender + designation + color,
    data = wine_train
  )

summary(ols_from_bkwd_fit_tukey)
summary(ols_from_bkwd_fit_tukey)$r.squared

#Predictions Backwards Fit Tukey----

preds_bkwd_tukey <- -predict(ols_from_bkwd_fit_tukey)
preds_bkwd_tukey

mod6_df <- data.frame(pred = preds_bkwd_tukey,
                      actual = wine_train$price)
##Plot of Preds Vs Actual

ggplot(mod6_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

mod6_df <- data.frame(pred = preds_bkwd_tukey,
                      actual = wine_train$price,
                      resids = ols_from_bkwd_fit_tukey$residuals)


##Plot of Resids vs Preds

ggplot(mod6_df, aes(x = pred, y = resids)) +
  geom_point(color = "purple", alpha = 1 / 100) + ggtitle("BKWD FIT TUKEY RESIDS VS PREDS") + geom_smooth() + theme_bw()

##RMSE

RMSE(mod6_df$pred, wine_train$price)

##MAE

MAE(mod6_df$pred, wine_train$price)
