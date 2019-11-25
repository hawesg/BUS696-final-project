#Source Libraries----

install.packages('rcompanion')

library('partykit')
library('rpart')
library('rpart.plot')
library('leaps')
library('tidyverse')
library('caret')
library('rcompanion')
library('MASS')

#Import Data----
load(here::here("data", "output", "clean_wine.RData"))
set.seed(1861)
options(scipen = 50)
train_idx <-
  sample(1:nrow(wine_data_clean), size = floor(nrow(wine_data_clean) * .75))
wine_train <- wine_data_clean %>% slice(train_idx)
wine_test <- wine_data_clean %>% slice(-train_idx)

#Mutate Data For Price Categories ----

wine_data_clean_tree <-
  wine_train %>%  mutate (price_lump = cut(
    wine_train$price,
    breaks = c(0, 4, 12, 50, 200, 750, 1000000),
    labels = c(
      "Budget ($(0-4)",
      "Every Day ($5-12)",
      "Premium ($13-50)",
      "Ultra Premium ($51-200)",
      "Luxury ($201-750)",
      "WTF is wrong with you? ($751+)"
    )
  ))

#Create R-Part Tree With Country Lump and Color Lump ----

form <- as.formula(price_lump ~ country_lump + color_lump)
tree.1 <-
  rpart(form, data = wine_data_clean_tree, control = rpart.control(minsplit =
                                                                     20, cp = 0))
prp(tree.1)

#Basic Decision Tree Price Model ----

decision_tree5 <-
  ctree(price_lump ~ point_cat, data = wine_data_clean_tree)

plot(decision_tree5, main = "Price Lump Vs Point_Cat")

#RPart Model (Price Lump and Point Cat) ----

wine_data_mod_tree <- rpart(
  price_lump ~ point_cat,
  data = wine_data_clean_tree,
  method = "class",
  control = list(cp = 0,
                 minsplit = 10)
)

rpart.plot(
  wine_data_mod_tree,
  box.palette = "RdBu",
  shadow.col = "0",
  clip.right.labs = TRUE,
  varlen = 0,
  clip.facs = TRUE,
  extra = 101,
  under = TRUE,
  main = "Price Lump and Point Cat"
)
print(wine_data_mod_tree)
wine_data_mod_tree$cptable
summary(wine_data_mod_tree)

#Attempt at One with Country_Lump ----

wine_data_mod_tree2 <- rpart(
  price_lump ~ country_lump,
  data = wine_data_clean_tree,
  method = "anova",
  control = list(cp = 0.001,
                 minsplit = 10)
)

rpart.plot(
  wine_data_mod_tree2,
  box.palette = "RdBu",
  shadow.col = "0",
  clip.right.labs = TRUE,
  varlen = 0,
  clip.facs = TRUE,
  extra = 100,
  under = TRUE,
  main = "Price Lump and Country Lump"
)

#Random Simple Trees (Also Mutate Country to 5 Lump) ----

wine_data_clean_treec <-
  wine_train %>% mutate(country_lump2 = fct_lump(country, 5))

decision_tree <- ctree(point_cat ~ color_lump,
                       data = wine_train)


decision_tree2 <- ctree(point_cat ~ taster_gender,
                        data = wine_train)

decision_tree3 <- ctree(point_cat ~ taster_name_lump,
                        data = wine_train)

decision_tree4 <- ctree(point_cat ~ country_lump2,
                        data = wine_data_clean_treec)

decision_tree5 <-
  ctree(price_lump ~ point_cat, data = wine_data_clean_tree)

decision_tree6 <-
  ctree(price_lump ~ winery_lump, data = wine_data_clean_tree)

decision_tree6 <-
  ctree(price_lump ~ winery_lump, data = wine_data_clean_tree)


plot(decision_tree)
plot(decision_tree2)
plot(decision_tree3)
plot(decision_tree4)
plot(decision_tree5)
plot(decision_tree6)

#Forward Fit Model ----

fit_fwd <-
  regsubsets(
    log(price) ~ country_lump +
      variety_lump +
      points +
      title_length +
      title_has_accents +
      designation_lump +
      taster_gender +
      taster_twitter_lump +
      #taster_name_lump +
      #color_lump +
      taster_review_count +
      taster_n_tweets +
      title_sentement +
      title_word_count +
      taster_avg_points +
      winery_lump,
    data = wine_train,
    method = "forward",
    nvmax = 10
  )

summary(fit_fwd)

plot(fit_fwd, scale = "adjr2", main = "Forward Fit Model")
coef(fit_fwd, 10)

ols_from_fwd_fit <-
  lm(
    log(price) ~ country_lump + variety_lump + points + taster_twitter_lump + designation_lump,
    data = wine_train
  )

summary(ols_from_fwd_fit)

#Predictions and Resids (With Plots) ----

preds <- predict(ols_from_fwd_fit)

mod1_df <- data.frame(pred = preds,
                      actual = wine_train$price)

ggplot(mod1_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

mod1_df <- data.frame(pred = preds,
                      actual = wine_train$price,
                      resids = ols_from_fwd_fit$residuals)

mod2_df <- data.frame(
  pred = exp(preds),
  actual = exp(wine_train$price),
  resids = -exp(preds) + exp(wine_train$price)
  
)

ggplot(mod1_df, aes(x = preds, y = resids)) + geom_point(color = "purple")

ggplot(mod2_df, aes(x = pred, y = resids)) + geom_point(color = "purple")

RMSE(mod2_df$pred, wine_train$price)

#Backward Fit Model ----

bkwd_fwd <-
  regsubsets(
    log(price) ~ country_lump +
      variety_lump +
      points +
      title_length +
      title_has_accents +
      designation_lump +
      taster_gender +
      taster_twitter_lump +
      #taster_name_lump +
      #color_lump +
      taster_review_count +
      taster_n_tweets +
      title_sentement +
      title_word_count +
      taster_avg_points +
      winery_lump,
    data = wine_train,
    method = "backward",
    nvmax = 10
  )

summary(bkwd_fwd)

plot(bkwd_fwd, scale = "adjr2", main = "Backward Fit Model")
coef(bkwd_fwd, 10)

ols_from_bkwd_fit <-
  lm(
    log(price) ~ country_lump + variety_lump + points + taster_twitter_lump + taster_review_count + designation_lump,
    data = wine_train
  )

summary(ols_from_bkwd_fit)


#Predictions and Resids (W/Plots)----

preds <- predict(ols_from_bkwd_fit)

mod3_df <- data.frame(pred = preds,
                      actual = wine_train$price)

ggplot(mod3_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

mod3_df <- data.frame(pred = preds,
                      actual = wine_train$price,
                      resids = ols_from_bkwd_fit$residuals)

mod4_df <- data.frame(
  pred = exp(preds),
  actual = exp(wine_train$price),
  resids = -exp(preds) + exp(wine_train$price)
  
)


ggplot(mod3_df, aes(x = pred, y = resids)) +
  geom_point(color = "purple")


ggplot(mod4_df, aes(x = pred, y = resids)) +
  geom_point(color = "purple")

RMSE(mod4_df$pred, wine_train$price)

#Tukey Experimentation----

sample <- sample(wine_train$price,size = 5000)
Tukey <- transformTukey(sample)
plotNormalHistogram(Tukey)

#Forward Fit Model-W/Tukey ----

fit_fwd_tukey <-
  regsubsets(
    -1 * price^(-.3) ~ country_lump +
      variety_lump +
      points +
      title_length +
      title_has_accents +
      designation_lump +
      taster_gender +
      taster_twitter_lump +
      #taster_name_lump +
      #color_lump +
      taster_review_count +
      taster_n_tweets +
      title_sentement +
      title_word_count +
      taster_avg_points +
      winery_lump,
    data = wine_train,
    method = "forward",
    nvmax = 10
  )

summary(fit_fwd_tukey)

plot(fit_fwd_tukey, scale = "adjr2", main = "Forward Fit Model")
coef(fit_fwd_tukey, 10)

ols_from_fwd_fit_tukey <-
  lm(
    -1 * price^(-.3) ~ country_lump + variety_lump + points + taster_twitter_lump + designation_lump,
    data = wine_train
  )

summary(ols_from_fwd_fit_tukey)

#Predictions----

preds <- -predict(ols_from_fwd_fit_tukey)
preds

mod5_df <- data.frame(pred = preds,
                      actual = wine_train$price)

ggplot(mod5_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

mod5_df <- data.frame(pred = preds,
                      actual = wine_train$price,
                      resids = ols_from_fwd_fit_tukey$residuals)

mod6_df <- data.frame(
  pred = exp(preds),
  actual = exp(wine_train$price),
  resids = -exp(preds) + exp(wine_train$price)
)


ggplot(mod5_df, aes(x = pred, y = resids)) +
  geom_point(color = "purple", alpha = 1/100)

ggplot(mod6_df, aes(x = pred, y = resids)) +
  geom_point(color = "purple")

RMSE(mod5_df$pred, wine_train$price)

#Backward Fit Model----

bkwd_fwd_tukey <-
  regsubsets(
    -1 * price^(-.3) ~ country_lump +
      variety_lump +
      points +
      title_length +
      title_has_accents +
      designation_lump +
      taster_gender +
      taster_twitter_lump +
      #taster_name_lump +
      #color_lump +
      taster_review_count +
      taster_n_tweets +
      title_sentement +
      title_word_count +
      taster_avg_points +
      winery_lump,
    data = wine_train,
    method = "backward",
    nvmax = 10
  )

summary(bkwd_fwd_tukey)

plot(bkwd_fwd_tukey, scale = "adjr2", main = "Backward Fit Model")
coef(bkwd_fwd_tukey, 10)

ols_from_bkwd_fit_tukey <-
  lm(
    -1 * price^(-.3) ~ country_lump + variety_lump + points + taster_twitter_lump + taster_review_count + designation_lump,
    data = wine_train
  )

summary(ols_from_bkwd_fit_tukey)
