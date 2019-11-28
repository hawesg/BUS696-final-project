
# wine_train <- wine_train[apply(is.na(wine_train),1,sum)==0,]
#wine_test <- wine_test[apply(is.na(wine_test),1,sum)==0,]


# getting the ratio from train data ---- 
# then we can see if same ratio can be used on the test data
wine_train.pt.min <- min(wine_train$points) 
price.min <- 2.5

# the following formula takes into consideration diminishing returns i.e. marginal increase in points is accompanied by a higher and higher increase in price
# also a lowest price for acceptable wine is set at $2.5 
# _
# Determining Good Value (i.e. well_priced)
### Need to consider diminishing returns, as the rate of marginal increase in points rating decreases with respect to the increase in the price of a bottle of wine, hence using log(price of wine):
##### Well_priced == whether we think wine is well priced
##### Well_priced will be determined according to a median price to points ratio computed as follows using observable from the training data set:
####### median_price_to_points_ratio= Dataset ratio numerator  / Dataset ratio denominator
########## Dataset ratio numerator    = Median (points awarded)  - Min (points awarded)
########## Dataset ratio denominator  = Median ( log ( price of wine))  - Min (log (expected(min price of drinkable wine))
############# Where expected(min price of drinkable wine) = $2.5 (i.e. educated guess)
############# Where price of wine > expected (min price of drinkable wine)
############# The following chart describes that relationship:

ratio.numerator <-  median(wine_train$points)-wine_train.pt.min
ratio.denominator <-  ifelse(wine_train$price<price.min, price.min + 0.1, wine_train$price)
ratio.denominator <- log(ratio.denominator)
ratio.denominator <-  median(ratio.denominator)
ratio.denominator <- ratio.denominator - log(price.min)
median_price_to_points_ratio <- ratio.numerator/ratio.denominator

.is_well_priced <- function(df){
  test.LHS.numerator <- df$points-wine_train.pt.min
  test.LHS.denominator <- ifelse(df$price<price.min, price.min + 0.1,df$price)
  test.LHS.denominator <- log(test.LHS.denominator)
  test.LHS.denominator <- test.LHS.denominator-log(price.min)
  test.LHS <- test.LHS.numerator/test.LHS.denominator
  test.RHS <- median_price_to_points_ratio
  well_priced <- factor(test.LHS > test.RHS, labels=c("No", "Yes"))
  return(well_priced)
}

head(wine_train)
# Compute well_priced for train ---- same formula as for median_price_to_points_ratio, except for an individual price point combination
wine_train_logit <- wine_train %>% 
  dplyr::mutate ( well_priced = .is_well_priced(.) ) %>%
  dplyr::select(-price,-points)

#view(wine_train_logit) regular scale ----
ggplot(wine_train_logit , aes(x = price, y = points, color = well_priced)) +
  geom_jitter() +
  theme(legend.position = "top") + 
  labs(title="Price and Points Colored by Well Priced", 
       color = "Well Priced") 

#view(wine_train_logit) log scale ----
ggplot(wine_train_logit , aes(y = price, x = points, color = well_priced)) +
  geom_jitter() + 
  theme(legend.position = "top") + 
  labs(title="Price and Points Colored by Well Priced", 
       color = "Well Priced", 
       caption="Price scaled log 10" ) + 
  scale_y_log10()

# Compute well_priced for test ----
wine_test_logit <- wine_test %>% 
  dplyr::mutate ( well_priced = .is_well_priced(.) )  %>% 
  select (-price,-points)

# let's create the model ----
## Since well_priced is a function of price and points, price and points are removed from the dataset (train and test) before model is created
## Otherwise same variables are used for the model so that it can be compared against other models

logit_mod <- glm( well_priced ~ .,
                  data = wine_train_logit %>%   
                    select (
                      -taster.twitter_handle,
                      -variety_and_color, 
                    ),
                  family = binomial) #our varaible can be 0 or 1, a binomial
# summary of model ----
summary(logit_mod)


### predictions -----
preds.train<- data.frame (
  pred = predict(logit_mod, type="response"),
  wine_train_logit
  #actual = wine_train_logit$well_priced
) 
head(preds.train)


# TODO FIX WARNING In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  : prediction from a rank-deficient fit may be misleading 

## test set
preds.test<- data.frame (
  pred = predict(logit_mod, 
                 newdata=wine_test_logit, 
                 type="response"),
  wine_test_logit
  #actual = wine_test_logit$well_priced
) 
head(preds.test)


#### ROC Curve ----
#head(preds.test)

TrainDF <- data.frame(default = c(preds.train$well_priced),
                      scores = c(preds.train$pred),
                      models = c(rep("Train Data Set",length(preds.train$pred))))

summary(TrainDF)

TestDF <- data.frame(default = c(preds.test$well_priced),
                     scores = c(preds.test$pred),
                     models = c(rep("Test Data Set",length(preds.test$pred))))


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

