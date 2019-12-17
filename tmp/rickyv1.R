#Source Libraries----

library('partykit')
library('rpart')
library('rpart.plot')
library('leaps')
library('tidyverse')
library('caret')
library('rcompanion')

#Import Data----

load(here::here("data", "output", "limited_factors", "data.train.RData"))
load(here::here("data", "output", "limited_factors", "wine_test.RData"))

#Standardize----
wine_data_to_be_standardized <-
  data.train %>% select(
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
  data.train %>% select(
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

data.train <- wine_data_standardized


#OlS Model (No Log Price) ----

model.ols.price <-
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
    data = data.train,
  )

summary(model.ols.price)
summary(model.ols.price)$r.squared

#Predictions-No Log Model ----

model.ols.price.preds <- predict(model.ols.price)
model.ols.price.preds

model.ols.price.preds_df <- data.frame(pred = model.ols.price.preds,
                       actual = data.train$price)

##Plot of Actual Vs Preds

ggplot(model.ols.price.preds_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

model.ols.price.preds_df <- data.frame(pred = model.ols.price.preds,
                       actual = data.train$price,
                       resids = model.ols.price$residuals)


##Plots of Preds Vs Resids

ggplot(model.ols.price.preds_df, aes(x = pred, y = resids)) + geom_point(color = "purple", alpha = 1 /
                                                           100) + ggtitle("OLS_NO_LOG RESIDS VS PREDS") + geom_smooth() + theme_bw()


#RMSE-OLS No Log

RMSE(model.ols.price.preds_df$pred, data.train$price)

#MAE OLS No Log

MAE(model.ols.price.preds_df$pred, data.train$price)


#Forward Fit Model (log price) ----

model.ols.log.step.fwd <-
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
    data = data.train,
    method = "forward",
    nvmax = 10
  )

summary(model.ols.log.step.fwd)

plot(model.ols.log.step.fwd, scale = "adjr2", main = "FORWARD SELECTION PROCEDURE")
coef(model.ols.log.step.fwd, 10)

##OLS Model Based on Fwd Fit

model.ols.log.from_fwd <-
  lm(log(price) ~ province + color + points + taster.name + taster.gender + designation,
     data = data.train)

summary(model.ols.log.from_fwd)
summary(model.ols.log.from_fwd)$r.squared

#Predictions-Forward Fit Model ----

model.ols.log.from_fwd.preds <- predict(model.ols.log.from_fwd)
model.ols.log.from_fwd.preds

model.ols.log.from_fwd.preds_df <- data.frame(pred = model.ols.log.from_fwd.preds,
                      actual = data.train$price)

##Plot of Actual Vs Preds

ggplot(model.ols.log.from_fwd.preds_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

model.ols.log.from_fwd.preds_df <- data.frame(pred = model.ols.log.from_fwd.preds,
                      actual = data.train$price,
                      resids = model.ols.log.from_fwd$residuals)


##Plots of Preds Vs Resids

ggplot(model.ols.log.from_fwd.preds_df, aes(x = pred, y = resids)) + geom_point(color = "purple", alpha = 1 /
                                                          100) + ggtitle("FWD_FIT MODEL RESIDS VS PREDS") + geom_smooth() + theme_bw()


##RMSE

RMSE(model.ols.log.from_fwd.preds_df$pred, data.train$price)

##MAE

MAE(model.ols.log.from_fwd.preds_df$pred, data.train$price)

#Backward Fit Model (log price) ----

model.ols.log.step.bkw <-
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
    data = data.train,
    method = "backward",
    nvmax = 10
  )

summary(model.ols.log.step.bkw)

plot(model.ols.log.step.bkw, scale = "adjr2", main = "BACKWARD SELECTION PROCEDURE")
coef(model.ols.log.step.bkw, 10)

##OLS Model Based on Bkwd

model.ols.log.from_bkw <-
  lm(log(price) ~ variety + province + points + taster.name + designation + taster.gender,
     data = data.train)

summary(model.ols.log.from_bkw)
summary(model.ols.log.from_bkw)$r.squared
plot(model.ols.log.from_bkw)

#Predictions-Backwards Fit Model----

model.ols.log.from_bkw.preds <- predict(model.ols.log.from_bkw)
model.ols.log.from_bkw.preds

##New Dataframe W/Resids Not Changed

model.ols.log.from_bkw.preds_df <- data.frame(pred = model.ols.log.from_bkw.preds,
                      actual = data.train$price,
                      resids = model.ols.log.from_bkw$residuals)


##Plot of Preds Vs Resids

ggplot(model.ols.log.from_bkw.preds_df, aes(x = pred, y = resids)) +
  geom_point(color = "purple", alpha = 1 / 100) + ggtitle("BKWD FIT MODEL RESIDS VS PREDS") + geom_smooth() + theme_bw()


##RMSE

RMSE(model.ols.log.from_bkw.preds_df$pred, model.ols.log.from_bkw.preds_df$actual)

##MAE

MAE(model.ols.log.from_bkw.preds_df$pred, model.ols.log.from_bkw.preds_df$actual)

#Tukey Experimentation----

price_sample <- sample(data.train$price, size = 5000)
tukey.transform <- transformTukey(price_sample)
plotNormalHistogram(tukey.transform)

#Forward Fit Model-W/Tukey ----

model.ols.tukey.step.fwd <-
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
    data = data.train,
    method = "forward",
    nvmax = 10
  )

summary(model.ols.tukey.step.fwd)

plot(model.ols.tukey.step.fwd, scale = "adjr2", main = "Forward Fit Model")
coef(model.ols.tukey.step.fwd, 10)

model.ols.tukey.from_fwd <-
  lm(
    -1 * price ^ (-.3) ~ country + color + points + province + taster.gender + taster.name + designation,
    data = data.train
  )

summary(model.ols.tukey.from_fwd)
summary(model.ols.tukey.from_fwd)$r.squared

#Predictions Fwd Fit Tukey----
##Note: Made Predict Function Negative to Generate Positive Predictions

model.ols.tukey.from_fwd.preds <- predict(model.ols.tukey.from_fwd)
model.ols.tukey.from_fwd.preds

model.ols.tukey.from_fwd.preds_df <- data.frame(pred = model.ols.tukey.from_fwd.preds,
                      actual = data.train$price)

##Plot of Preds Vs Actual

ggplot(model.ols.tukey.from_fwd.preds_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

model.ols.tukey.from_fwd.preds_df <- data.frame(pred = model.ols.tukey.from_fwd.preds,
                      actual = data.train$price,
                      resids = model.ols.tukey.from_fwd$residuals)


##Plot of Resids vs Preds

ggplot(model.ols.tukey.from_fwd.preds_df, aes(x = pred, y = resids)) +
  geom_point(color = "purple", alpha = 1 / 100) + ggtitle("FWD FIT TUKEY RESIDS VS PREDS") + geom_smooth() + theme_bw()


##RMSE

RMSE(model.ols.tukey.from_fwd.preds_df$pred, data.train$price)

##MAE

MAE(model.ols.tukey.from_fwd.preds_df$pred, data.train$price)

#Backward Fit Model----

model.ols.tukey.step.bkw <-
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
    data = data.train,
    method = "backward",
    nvmax = 10
  )

summary(model.ols.tukey.step.bkw)

plot(model.ols.tukey.step.bkw, scale = "adjr2", main = "Backward Fit Model")
coef(model.ols.tukey.step.bkw, 10)

model.ols.tukey.from_bkw <-
  lm(
    -1 * price ^ (-.3) ~ country + variety + points + province + taster.gender + designation + color,
    data = data.train
  )

summary(model.ols.tukey.from_bkw)
summary(model.ols.tukey.from_bkw)$r.squared
as.formula(model.ols.tukey.from_bkw)
coef(model.ols.tukey.from_bkw) %>% round(2)
cplot(model.ols.tukey.from_bkw)

#Predictions Backwards Fit Tukey----

model.ols.tukey.from_bkw.preds <- predict(model.ols.tukey.from_bkw)
model.ols.tukey.from_bkw.preds

model.ols.tukey.from_bkw.preds_df <- data.frame(pred = model.ols.tukey.from_bkw.preds,
                      actual = data.train$price,
                      resids = model.ols.tukey.from_bkw$residuals)


##Plot of Resids vs Preds

ggplot(model.ols.tukey.from_bkw.preds_df, aes(x = pred, y = resids)) +
  geom_point(color = "purple", alpha = 1 / 100) + ggtitle("BKWD FIT TUKEY RESIDS VS PREDS") + geom_smooth() + theme_bw()

##RMSE

RMSE(model.ols.tukey.from_bkw.preds_df$pred, model.ols.tukey.from_bkw.preds_df$actual)

##MAE

MAE(model.ols.tukey.from_bkw.preds_df$pred, model.ols.tukey.from_bkw.preds_df$actual)
