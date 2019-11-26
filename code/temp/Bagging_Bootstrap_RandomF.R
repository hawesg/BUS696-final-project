# Load most commonly used libraries


# libarries to include note -----------------------------------------------


# need to include: ElemStatLearn + partykit + randomForest + randomForestExplainer + analogue


list.of.packages <- c("tidyverse", "plotly", "here", "ggthemes", "stringr", "plyr", "stringi", "readxl",".")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.


library("conflicted")
library("tidyverse")
library("here")
library("ggthemes")
library("stringr")
library("stringi")
library("readxl")
library("ggExtra")
library("PerformanceAnalytics")
library('sentimentr')
library('ElemStatLearn')
library('partykit')
library('magrittr')
library('randomForest')
library('randomForestExplainer')
library('analogue')

conflict_prefer("mutate", "dplyr")


# Load data 100 (could use other) -----------------------------------------

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
load(here::here("data","output//more_than_100_obs","clean_wine.RData"))

#wine_data_clean <- wine_data

names(wine_data_clean)


# Seeding, not needed with Garrett's code ---------------------------------


set.seed(1861)

# Train sample only at 0.10 for testing -----------------------------------
trainSize <- 0.10
train_idx <- sample(1:nrow(wine_data_clean), size = floor(nrow(wine_data_clean) * trainSize))


# Bootstrap: Selecting Specific Columns that work  -----------------------

wine_data_clean_rm_model <- wine_data_clean %>%
  #mutate(price=log(price)) %>% 
  select (
    price,
    points,
    #points.category,  # cannot use it along with points
    country,
    # province, #breaks bootstrap
    color,
    #variety,  #breaks bootstrap
    winery,
    taster.gender, 
    taster.avg_points,
    #variety_and_color,  #breaks bootstrap
    title.n_words,
    title.n_chars,
    title.sentement,
    title.has_accents
  ) 

Model_train <- wine_data_clean_rm_model %>% slice(train_idx)
Model_test <- wine_data_clean_rm_model %>% slice(-train_idx)

names(Model_train)

# remove n/a values
Model_train <- Model_train[apply(is.na(Model_train),1,sum)==0,]
Model_test <- Model_test[apply(is.na(Model_test),1,sum)==0,]

# store rownames as columns
Model_train_preds <- Model_train %>% rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))

# bagging - bootstrapp aggregation
B <- 30      # number of bootstrap samples
num_b <- 1000  # sample size of each bootstrap
boot_mods <- list() # store our bagging models
for(i in 1:B){
  boot_idx <- sample(1:nrow(Model_train), 
                     size = num_b,
                     replace = FALSE)
  # fit a tree on each bootstrap sample
  data_slice = Model_train %>%     slice(boot_idx)
  
  # Log(price) bootstrap model ----
  boot_tree <- ctree(log(price) ~ ., 
                     data = data_slice) 
  # store bootstraped model
  boot_mods[[i]] <- boot_tree
  # generate predictions for that bootstrap model
  preds_boot <- data.frame(
    preds_boot = predict(boot_tree),
    #resid =  data_slice$price - predict(boot_tree),
    rowname = boot_idx 
  )  
  
  
  # rename prediction to indicate which boot iteration it came from
  names(preds_boot)[1] <- paste("preds_boot",i,sep = "")
  
  # merge predictions to Model_train dataset
  Model_train_preds <- left_join(x = Model_train_preds, y = preds_boot,
                                 by = "rowname")
}

names(Model_train_preds)
## Examine individual models, will need to spend more time here ----
plot(boot_mods[[1]])
plot(boot_mods[[2]])
plot(boot_mods[[3]])
plot(boot_mods[[4]])
plot(boot_mods[[5]])
plot(boot_mods[[6]])
plot(boot_mods[[7]])
plot(boot_mods[[8]])
plot(boot_mods[[9]])
plot(boot_mods[[10]])

# must convert factor into numeric, note that class "0" = 1, 
# and class "1" = 2, so we need to subtract 1 from every column
Model_train_preds %<>% mutate_if(is.factor, as.numeric) %>% 
  mutate_all(function(x){x - 1})

# calculate mean over all the bootstrap predictions
Model_train_preds %<>% mutate(preds_bag = 
                                select(., preds_boot1:preds_boot30) %>% 
                                rowMeans(na.rm = TRUE))

# congratulations! You have bagged your first model!

# plot bagged model -------------------------------------------------------
ggplot(Model_train_preds, aes(x = preds_bag)) + geom_histogram()

#residuals(Model_train_preds, which = c("model", "bootstrap"))

#############################################+
# Random Forest -----------------------------------------------------------
#############################################+

# rf_fit <- randomForest(price ~ . ,
#                        data = Model_train,
#                        type = classification,
#                        mtry = 3,
#                        ntree = 1000,
#                        importance = TRUE,
#                        localImp = TRUE)

rf_fit <- randomForest(log(price) ~ . ,
                       data = Model_train,
                       importance = TRUE,
                       localImp = TRUE)

# RForest model review ------
rf_fit

# Gini Coefficient --------------------------------------------------------
rf_fit$importance
# Plot variable vs IncNodePurity ----
# IncNodePurity - Total decrease in node impurities from splitting on the variable, averaged over all trees. Impurity is measured by residual sum of squares. Impurity is calculated only at node at which that variable is used for that split. Impurity before that node, and impurity after the split has occurred.
# here points and then title.n_chars matter most
varImpPlot(rf_fit,type=2)

#############################################+
# Explaining Random Forests ----
#############################################+

# install.packages('randomForestExplainer')
library(randomForestExplainer)
# # plot min
# plot_min_depth_distribution(rf_fit)
# 
# plot_multi_way_importance(rf_fit)
# 
# plot_multi_way_importance(rf_fit, x_measure = "mse_increase",
#                           y_measure = "node_purity_increase")
# # 
# plot_predict_interaction(rf_fit, Model_train, "taster.avg_points", "points")
# 
# # relations between measure of importance
# plot_importance_ggpairs(rf_fit)

# RForest Explanation file render ---- 
explain_forest(rf_fit, interactions = TRUE, data = Model_train)


# prediction
# pred = predict(rf_fit, newdata=Model_test)
# summary(pred)

### RForest predictions and residuals -----
preds_trainset<- data.frame (
  scores_rf = predict(rf_fit,newdata=Model_train),
  residuals = Model_train$price - exp(predict(rf_fit,newdata=Model_train)),
  Model_train
) 
summary (preds_trainset$residuals)

# Plot these residuals maybe? ---------------------------------------------
#plot(preds_testset)

## test set
preds_testset<- data.frame (
  scores_rf = predict(rf_fit,newdata=Model_test),
  residuals = Model_test$price - exp(predict(rf_fit,newdata=Model_test)),
  Model_test
) 
summary (preds_testset$residuals)
# Plot these residuals maybe? ---------------------------------------------
#plot(preds_testset)

#---------------------------------------------------------------+
# Tuning Random Forests - not too sure what to do here ----
#---------------------------------------------------------------+

rf_mods <- list()
oob_err <- NULL
test_err <- NULL
for(mtry in 1:9){
  rf_fit <- randomForest(price ~ ., 
                         data = Model_train,
                         mtry = mtry #,
                         #ntree = 500
                         )
  oob_err[mtry] <- rf_fit$err.rate[500]
  
  cat(mtry," ")
}

results_DF <- data.frame(mtry = 1:9,oob_err)

ggplot(results_DF, aes(x = mtry, y = oob_err)) + geom_point()

