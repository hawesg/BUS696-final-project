
################################################################################
#                                                                              #
# Purpose:       Bagging Models                                                #
#                                                                              #
# Author:        Garrett H, Bart C, Ricky L                                    #
# Contact:       hawes102@mail.chapman.edu                                     #
# Client:        Jonathan Hersh                                                #
#                                                                              #
# Code created:  2019-12-16                                                    #
# Last updated:  2019-12-16                                                    #
# Source:        /Users/garretthawes/wine-project                              #
#                                                                              #
# Comment:       These are the two seperate bootstrap models first the good    #
#                one using caret and then the one based on code from class     #
#                that doesn't work very well.                                  #
#                                                                              #
################################################################################

##################### More robust model using caret train ######################

library(doParallel)
library(doParallel)  # for parallel backend to foreach
library(foreach)     # for parallel processing with for loops

# Modeling packages
library(caret)       # for general model fitting
library(rpart)       # for fitting decision trees
library(ipred)       # for fitting bagged decision trees

cl <- makeCluster(4) # use 4 workers
registerDoParallel(cl) # register the parallel backend

model.bootstrap.advanced <- train(
  -1 * (price ^(-.3)) ~ .,
  data = data.train,
  method = "treebag",
  trControl = trainControl(method = "cv", number = 10),
  nbagg = 200,  
  control = rpart.control(minsplit = 2, cp = 0)
)
pushover(message = msg, 
         user = userID, 
         app = appToken)


# predictions <- foreach(
#   icount(100), 
#   .packages = "rpart", 
#   .combine = cbind ) %dopar% {
#     # bootstrap copy of training data
#     index <- sample(nrow(data.train), replace = TRUE)
#     wine_train_boot <- data.train[index, ]  
#     
#     # fit tree to bootstrap copy
#     bagged_tree <- rpart(
#       -1 * (price ^(-.3)) ~ ., 
#       control = rpart.control(minsplit = 2, cp = 0),
#       data = data.train
#     ) 
#     
#     predict(bagged_tree, newdata = data.test)
#   }
# 
# 
# predictions %>%
#   as.data.frame() %>%
#   mutate(
#     observation = 1:n(),
#     actual = -1 * (data.test$price ^(-.3))) %>%
#   tidyr::gather(tree, predicted, -c(observation, actual)) %>%
#   group_by(observation) %>%
#   mutate(tree = stringr::str_extract(tree, '\\d+') %>% as.numeric()) %>%
#   ungroup() %>%
#   arrange(observation, tree) %>%
#   group_by(observation) %>%
#   mutate(avg_prediction = cummean(predicted)) %>%
#   group_by(tree) %>%
#   summarize(RMSE = RMSE(avg_prediction, actual)) %>%
#   ggplot(aes(tree, RMSE)) +
#   geom_line() +
#   xlab('Number of trees') + theme_solarized()

stopCluster(cl)

bag_test_pred <- predict(model.bootstrap.advanced, newdata = data.test)

head(.tukey(data.test$price))

bag_pred_test_df <- data.frame(actual = .tukey(data.test$price), pred = bag_test_pred) 


postResample(bag_pred_test_df$pred, bag_pred_test_df$actual)

################################ In class model ################################

# store rownames as columns
model.bootstrap.preds <- data.train %>% 
  rownames_to_column() %>% 
  mutate( rowname = as.numeric(rowname) ) %>% 
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
    title.has_accents,
    rowname
  ) 

glimpse(model.bootstrap.preds)

# bagging - bootstrapp aggregation
# Bootstrap model
# Created 30 samples with 1000 data points each (run into performance issues otherwise)
# Carefully selected variables, to allows bootstrapping
B <- 100      # number of bootstrap samples
num_b <- 500  # sample size of each bootstrap
boot_mods <- list() # store our bagging models
for(i in 1:B){
  boot_idx <- sample(1:nrow(data.train), 
                     size = num_b,
                     replace = FALSE)
  # fit a tree on each bootstrap sample
  data_slice = data.train %>% slice(boot_idx)
  
  # Log(price) bootstrap model ----
  boot_tree <- ctree(log(price)~ ., 
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
  
  # merge predictions to data.train dataset
  model.bootstrap.preds <- left_join(x = model.bootstrap.preds, y = preds_boot,
                                by = "rowname")
}

# must convert factor into numeric, note that class "0" = 1, 
# and class "1" = 2, so we need to subtract 1 from every column
model.bootstrap.preds %<>% 
  mutate_if(is.factor, as.numeric) %>% 
  mutate_all(function(x){x - 1})

# calculate mean over all the bootstrap predictions
model.bootstrap.preds <- model.bootstrap.preds %>%
  mutate(preds_bag = select(., preds_boot1:preds_boot30) %>% 
                               rowMeans(na.rm = TRUE))


head(model.bootstrap.preds)

# plot bagged model -------------------------------------------------------
# ggplot(model.bootstrap.preds, aes(x = exp(preds_bag))) + geom_histogram()
# TODO FIX THIS SINCE exp no longer works
ggplot(model.bootstrap.preds, aes(x = preds_bag)) + geom_histogram()
summary(model.bootstrap.preds)

model.bootstrap.preds$preds_bag
