# commentr::header_comment("Alternate aproach to look at logistic regression", "This is based on a different set of metrics to examine classification problems.", "Garrett Hawes", "hawes102@mail.chapman.edu")

###############################################################################-
#                                                                              #
# Purpose:       Alternate aproach to look at logistic regression              #
#                                                                              #
# Author:        Garrett Hawes                                                 #
# Contact:       hawes102@mail.chapman.edu                                     #
# Client:        Garrett Hawes                                                 #
#                                                                              #
# Code created:  2019-12-03                                                    #
# Last updated:  2019-12-03                                                    #
# Source:        /Users/garretthawes/wine-project                              #
#                                                                              #
# Comment:       This is based on a different set of metrics to examine class  #
#                ification problems.                                           #
#                                                                              #
###############################################################################-



data.train.pt.min <- min(data.train$points) 
price.min <- 2.5

# the following formula takes into consideration diminishing returns i.e. marginal increase in points is accompanied by a higher and higher increase in price
# also a lowest price for acceptable wine is set at $2.5 

ratio.numerator <-  median(data.train$points)-data.train.pt.min



ratio.denominator <- ifelse(data.train$price<price.min, price.min + 0.1, data.train$price)
ratio.denominator <- log(ratio.denominator)
ratio.denominator <-  median(ratio.denominator)
ratio.denominator <- ratio.denominator - log(price.min)

median_price_to_points_ratio <- ratio.numerator/ratio.denominator

# cleanup
rm(ratio.numerator,ratio.denominator)

############################### HELPER FUNCTION ################################

.is_well_priced <- function(df){
  test.LHS.numerator <- df$points-data.train.pt.min
  test.LHS.denominator <- ifelse(df$price<price.min, price.min + 0.1,df$price)
  test.LHS.denominator <- log(test.LHS.denominator)
  test.LHS.denominator <- test.LHS.denominator-log(price.min)
  test.LHS <- test.LHS.numerator/test.LHS.denominator
  test.RHS <- median_price_to_points_ratio
  well_priced <- factor(test.LHS > test.RHS, labels=c("No", "Yes"))
  return(well_priced)
}

# Compute well_priced for train ---- same formula as for median_price_to_points_ratio, except for an individual price point combination
data.train_logit <- data.train %>% 
  dplyr::mutate ( well_priced = .is_well_priced(.) ) %>%
  dplyr::select(-price,-points)


#view(data.train_logit) regular scale ----
ggplot(data.train_logit , aes(x = data.train$price, y = data.train$points, color = well_priced)) +
  geom_jitter() +
  theme(legend.position = "top") + 
  labs(title="Price and Points Colored by Well Priced", 
       color = "Well Priced") 

#view(data.train_logit) log scale ----
ggplot(data.train_logit , aes(y = data.train$price, x = data.train$points, color = well_priced)) +
  geom_jitter() + 
  theme(legend.position = "top") + 
  labs(title="Price and Points Colored by Well Priced", 
       color = "Well Priced", 
       caption="Price scaled log 10" ) + 
  scale_y_log10()

# Compute well_priced for test ----
data.test_logit <- data.test %>% 
  dplyr::mutate ( well_priced = .is_well_priced(.) )  %>% 
  select (-price, -points)

# let's create the model ----
## Since well_priced is a function of price and points, price and points are removed from the dataset (train and test) before model is created
## Otherwise same variables are used for the model so that it can be compared against other models

model.logit <- glm( well_priced ~ . - variety_and_color,
                  data = data.train_logit,
                  family = binomial) #our varaible can be 0 or 1, a binomial

# as.formula(data.train %>% select)
# summary of model ----
summary(model.logit)

names(data.test)
### predictions -----
preds.train<- data.frame (
  pred = predict(model.logit, type="response"),
  data.train_logit
  #actual = data.train_logit$well_priced
) 


preds.train <- data.frame (
  pred = predict(model.logit, type="response"),
  actual = data.train_logit$well_priced
)
head(preds.train)


## test set
preds.test<- data.frame (
  pred = predict(model.logit, 
                 newdata=data.test_logit, 
                 type="response"),
  #data.test_logit
  actual = data.test_logit$well_priced
) 
head(data.test_logit)
head(preds.test)
glimpse(data.test_logit)

#### ROC Curve ----
#head(preds.test)

TrainDF <- data.frame(default = preds.train$actual,
                      scores = preds.train$pred,
                      models = c(rep("Train Data Set",length(preds.train$pred))))

# summary(TrainDF)

TestDF <- data.frame(default = preds.test$actual,
                     scores = preds.test$pred,
                     models = c(rep("Test Data Set",length(preds.test$pred))))

test <- TrainDF %>% bind_rows(TestDF)

test <- test %>% mutate(models = factor(models))
head(test)
str(test)



### ROC Curve train -----
TrainROC <- ggplot(TrainDF, aes(m = scores, d = default, color = models)) + 
  geom_roc(show.legend = TRUE, labelsize = 3.5, cutoffs.at = c(.99,.9,.8,.7,.5,.3,.1,0))
TrainROC <- TrainROC + style_roc(theme = theme_solarized_2()) +
  theme(axis.text = element_text(colour = "blue")) +
  theme(legend.justification = c(1, 0), 
        legend.position = c(1, 0),
        legend.box.margin=margin(c(50,50,50,50)))
plot(TrainROC)

### ROC Curve test ----
TestROC <- ggplot(TestDF, aes(m = scores, d = default, color = models)) + 
  geom_roc(show.legend = TRUE, labelsize = 3.5, cutoffs.at = c(.99,.9,.8,.7,.5,.3,.1,0))
TestROC <- TestROC + style_roc(theme = theme_solarized_2()) +
  theme(axis.text = element_text(colour = "blue")) +
  theme(legend.justification = c(1, 0), 
        legend.position = c(1, 0),
        legend.box.margin=margin(c(50,50,50,50)))
plot(TestROC)

### Area under the curve ----
AUC_results <- data.frame (
  train = calc_auc(TrainROC),
  test =calc_auc(TestROC)
)
# AUC_results$TrainAUC.AUC <- round(AUC_results$TrainAUC.AUC,4)
# AUC_results$TestAUC.AUC <- round(AUC_results$TestAUC.AUC,4)

AUC_results <- AUC_results %>% select(train.AUC, test.AUC) %>% round(4)

AUC_results 

paste0("Test (",AUC_results$test.AUC,")")
paste0("Train (",AUC_results$train.AUC,")")

head(test)
levels(test$models)
test$models <- recode(test$models,`Test Data Set` = paste0("Test (",AUC_results$test.AUC,")"), `Train Data Set` = paste0("Train (",AUC_results$train.AUC,")"))


bothROC <- ggplot(test, aes(m = scores, d = default, color = models)) + 
  geom_roc(show.legend = TRUE, labelsize = 3.5, cutoffs.at = c(.99,.9,.8,.7,.5,.3,.1,0))
bothROC <- bothROC + style_roc(theme = theme_solarized_2()) +
  # theme(axis.text = element_text(colour = "blue")) +
  theme(legend.justification = c(1, 0), 
        legend.position = c(1, 0),
        legend.box.margin=margin(c(50,50,50,50)))+
  labs(title="ROC Plot for Test and Training Set",
       color="Data Set",
       caption="* AUC in brackets")
plot(bothROC)
