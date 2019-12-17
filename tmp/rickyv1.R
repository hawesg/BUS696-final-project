########################### OLS Untransformed price ############################

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

model.ols.price.preds_df <- data.frame(pred = predict(model.ols.price),
                                       actual = data.train$price) %>% mutate(resids = actual - pred)

##Plot of Actual Vs Preds

ggplot(model.ols.price.preds_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")


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


model.ols.log.from_fwd.preds_df <- data.frame(pred = predict(model.ols.log.from_fwd),
                      actual = log(data.train$price)) %>% mutate(resids = actual - pred)

##Plot of Actual Vs Preds

ggplot(model.ols.log.from_fwd.preds_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

##Plots of Preds Vs Resids

ggplot(model.ols.log.from_fwd.preds_df, aes(x = pred, y = resids)) + geom_point(color = "purple", alpha = 1 /
                                                          100) + ggtitle("FWD_FIT MODEL RESIDS VS PREDS") + geom_smooth() + theme_bw()


##RMSE

RMSE(model.ols.log.from_fwd.preds_df$pred, model.ols.log.from_fwd.preds_df$actual)

##MAE

MAE(model.ols.log.from_fwd.preds_df$pred, model.ols.log.from_fwd.preds_df$actual)

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

model.ols.log.from_bkw.preds <-
model.ols.log.from_bkw.preds

##New Dataframe W/Resids Not Changed

model.ols.log.from_bkw.preds_df <- data.frame(pred =predict(model.ols.log.from_bkw),
                      actual = data.train$price) %>% mutate(resids = actual-pred)

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

model.ols.tukey.from_fwd.preds <- 
model.ols.tukey.from_fwd.preds

model.ols.tukey.from_fwd.preds_df <- data.frame(pred = predict(model.ols.tukey.from_fwd),
                      actual = data.train$price) %>% mutate(resids = actual - pred)

##Plot of Preds Vs Actual

ggplot(model.ols.tukey.from_fwd.preds_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

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

model.ols.tukey.from_bkw.preds_df <- data.frame(pred = predict(model.ols.tukey.from_bkw),
                      actual = data.train$price) %>% mutate(resids = actual - pred)

##Plot of Resids vs Preds

ggplot(model.ols.tukey.from_bkw.preds_df, aes(x = pred, y = resids)) +
  geom_point(color = "purple", alpha = 1 / 100) + ggtitle("BKWD FIT TUKEY RESIDS VS PREDS") + geom_smooth() + theme_bw()

##RMSE

RMSE(model.ols.tukey.from_bkw.preds_df$pred, model.ols.tukey.from_bkw.preds_df$actual)

##MAE

MAE(model.ols.tukey.from_bkw.preds_df$pred, model.ols.tukey.from_bkw.preds_df$actual)
