#############################################+
# Random Forest -----------------------------------------------------------
#############################################+

# as.formula(data.train %>% select (
#   price,
#   points,
#   #points.category,  # cannot use it along with points
#   country,
#   # province, #breaks bootstrap
#   color,
#   #variety,  #breaks bootstrap
#   winery,
#   taster.gender, 
#   taster.avg_points,
#   #variety_and_color,  #breaks bootstrap
#   title.n_words,
#   title.n_chars,
#   title.sentement,
#   title.has_accents
# ) )
rf_fit <- randomForest(.tuky(price) ~ points + country + color + winery + taster.gender + taster.avg_points + 
                         title.n_words + title.n_chars + title.sentement + title.has_accents,
                       data = data.train,
                       importance = TRUE,
                       localImp = TRUE)

.tuky(data.train$price)

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
# plot_predict_interaction(rf_fit, data.train, "taster.avg_points", "points")
# 
# # relations between measure of importance
# plot_importance_ggpairs(rf_fit)

# RForest Explanation file render ---- 
explain_forest(rf_fit, interactions = TRUE, data = data.train)


# prediction
# pred = predict(rf_fit, newdata=wine_test)
# summary(pred)

### RForest predictions and residuals -----
.model_summary(predict(rf_fit,newdata=data.train),.tuky(model.train$price),"Random Forest Train")

.model_summary(predict(rf_fit,newdata=data.test),.tuky(model.train$price),"Random Forest Test")

# preds_trainset<- data.frame (
#   scores_rf = ,
#   residuals = data.train$price - exp(predict(rf_fit,newdata=data.train)),
#   data.train
# ) 
# summary (preds_trainset$residuals)

# Plot these residuals maybe? ---------------------------------------------
# plot(preds_testset)
# 
# ## test set
# preds_testset<- data.frame (
#   scores_rf = predict(rf_fit,newdata=wine_test),
#   residuals = wine_test$price - exp(predict(rf_fit,newdata=wine_test)),
#   wine_test
# ) 
# summary (preds_testset$residuals)
# # Plot these residuals maybe? ---------------------------------------------
# plot(preds_testset)

#---------------------------------------------------------------+
# Tuning Random Forests - this may take a while ----
#---------------------------------------------------------------+

# rf_mods <- list()
# oob_err <- NULL
# test_err <- NULL
# for(mtry in 1:9){
#   rf_fit <- randomForest(price ~ ., 
#                          data = data.train,
#                          mtry = mtry,
#                          ntree = 500
#   )
#   oob_err[mtry] <- rf_fit$err.rate[500]
#   
#   cat(mtry," ")
# }
# 
# results_DF <- data.frame(mtry = 1:9,oob_err)
# 
# ggplot(results_DF, aes(x = mtry, y = oob_err)) + geom_point()