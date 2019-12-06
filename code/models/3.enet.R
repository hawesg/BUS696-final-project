###############################################################################-
#                                                                              #
# Purpose:       Elastic Net Regression Model                                  #
#                                                                              #
# Author:        Garrett H, Bart C, Ricky L                                    #
# Contact:       hawes102@mail.chapman.edu                                     #
# Client:        Jonathan Hersh                                                #
#                                                                              #
# Code created:  2019-11-22                                                    #
# Last updated:  2019-11-22                                                    #
# Source:        /Users/garretthawes/wine-project                              #
#                                                                              #
# Comment:       Mostly a starting off point but was able to get R2 of .55 or  #
#                so                                                            #
#                                                                              #
###############################################################################-

###############################################################################-
#                                                                              #
#  Copy this stuff into the top of any script you are working on for modeling  #
#                                                                              #
###############################################################################-

# rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

###############################################################################-

################################## Libraries ###################################
#                                                                              #
#  Omit this if you want to just load them manually                            #
#                                                                              #
###############################################################################-
#
source("code/libraries.R")

# names(data.train)

################################# Class method #################################

# using length 100
alpha_list <- seq(0, 1, len = 101)
alpha_list

#match(alpha_list,0.47)

library(glmnet)
library(glmnetUtils)

names(data.train)

#get the model
model.enet <- cva.glmnet(
  -1 * price ^ (-.3) ~ .,
  data = data.train %>% select(
   -variety_and_color
  ),
  trace.it=1
  ,
  alpha = alpha_list
)

print(model.enet)

summary(model.enet)
############## Helper function, minlossplot with additional info ###############

.mlossp <- function (x, ..., cv.type = c("1se", "min"))
{
  alpha <- x$alpha
  cv.type <- match.arg(cv.type)
  cv.type <- paste0("lambda.", cv.type)
  cvm <- sapply(x$modlist, function(mod) {
    mod$cvm[mod$lambda == mod[[cv.type]]]
  })
  min_cv <- which.min(cvm)
  plot(alpha,
        cvm,
        ylab = "CV loss",
        xlab = paste0("alpha (min ", alpha[min_cv], ")"),
        ...)
  points(
    x = alpha[min_cv],
    y = cvm[min_cv],
    pch = 20,
    col = "red",
    bg = "yellow",
    cex = 0.90
  )
  text(x=alpha[min_cv]+.04, y = cvm[min_cv], paste0("    alpha=",alpha[min_cv]), cex = .8)
  invisible(x)
}

### minlossplot
minlossplot(model.enet)
.mlossp(model.enet)
cvm <- sapply(model.enet$modlist, function(mod) {
  mod$cvm[mod$lambda == mod[['lambda.1se']]]
})
min_cv <- which.min(cvm)
alpha_list[min_cv]

# from the above the optimal elasticnet model is at alpha 0.47, this is more towards ridge
# (alpha =0) and less towards lasso (alpha=1); but it kind of lands in the middle so it is the
# best of both worlds
# best value at alpha =0.47 where the cross validation loss is the lowest

# .47 is element 48

plot(model.enet$modlist[[min_cv]])

model.enet.best <- model.enet$modlist[[min_cv]]

summary(model.enet.best)
# library("ggfortify")
# autoplot(, colour = 'blue', which = 1:6)

# # other candidates
# plot(model.enet$modlist[[1]])  ## alphas zero, ridge model
# plot(model.enet$modlist[[20]])
# plot(model.enet$modlist[[28]]) ## best
# plot(model.enet$modlist[[42]])
# plot(model.enet$modlist[[66]])
# plot(model.enet$modlist[[101]]) ## alphas zero, lasso  model

# coefficient matrix for the optimal elasticnet model using lambda.1se
coef(model.enet, alpha = alpha_list[min_cv],
     s = model.enet$modlist[[min_cv]]$lambda.1se) %>% round(3)

########################### Framework to check preds ###########################



library("caret")

model.logit.test.preds.df <- data.frame(
  actual=  -1 * (data.train$price ^ (-.3)),
  pred = predict(model.enet, alpha = alpha_list[min_cv], lambda = lambda.min, data.train) %>% round(3)
) %>% rename(actual = 1, pred = 2) %>% remove_rownames() 


.model_summary(model.logit.test.preds.df$pred, model.logit.test.preds.df$actual, "Enet Fit")

.save_model_metrics("enet",
                    "Train", 
                    "-1 * price ^ (-.3)", 
                    ". -variety_and_color",
                    paste("Alpha:",alpha_list[min_cv],"Lambda: 1se"), 
                    model.logit.test.preds.df$pred, 
                    model.logit.test.preds.df$actual) 
metric.test


# ##Plot of Actual Vs Preds
# 
# ggplot(mod1_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
#   geom_abline(color = "red", linetype = "dashed")
# 
# mod1_df <- data.frame(pred  = model.logit.test.preds.df$pred,
#                       actual = model.logit.test.preds.df$actual,
#                       resids = model.logit.test.preds.df$actual-model.logit.test.preds.df$pred )
# 
# mod1_df
# 
# 
# ##Plots of Preds Vs Resids
model.enet.results <- data.frame(actual = .tukey(data.train$price),
                                 pred = predict(model.enet, alpha = alpha_list[min_cv], lambda = lambda.min, data.train)) %>% 
  rename(actual = 1, pred = 2) %>% 
  remove_rownames() %>%
  mutate( resid = actual - pred)

head(model.enet.results)
postResample(pred = model.enet.results$pred, obs = model.enet.results$actual)

ggplot(model.enet.results,aes(x = pred, y = resid)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs") ) +
  labs(x="Fitted values", 
       y="Residuals", 
       title="Residuals vs Fitted Plot", 
       caption = "Lamda: 1se, Alpha: 0.38", 
       subtitle = "R2:0.597 | RMSE: 0.045 | MAE: 0.036")+
  geom_hline(yintercept=0, col="red", linetype="dashed")+
  theme_bw()

# library('plotROC')
# roc_plot_train <- ggplot(model.logit.test.preds.df, aes(m = pred, d = actual)) + geom_roc() + ggtitle("Training Data ROC curve") + style_roc()
# roc_plot_train
