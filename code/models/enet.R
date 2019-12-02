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

if(!(exists("wine_train")&&exists("wine_train"))) {
  load(here::here("data","output","wine_train.RData"))
  load(here::here("data","output","wine_test.RData")) 
}

###############################################################################-

################################## Libraries ###################################
#                                                                              #
#  Omit this if you want to just load them manually                            #
#                                                                              #
###############################################################################-
#
source("code/libraries.R")

# names(wine_train)

# new_DF <- wine_test[rowSums(is.na(wine_test)) > 0,] %>% View()

################################# Class method #################################

# using length 100
alpha_list <- seq(0, 1, len = 101)
alpha_list

#match(alpha_list,0.47)

library(glmnet)
library(glmnetUtils)

names(wine_train)

#get the model
enet_fit <- cva.glmnet(
  -1 * price ^ (-.3) ~ .,
  data = wine_train %>% select(
   -variety_and_color
  ),
  trace.it=1
  ,
  alpha = alpha_list
)

print(enet_fit)

summary(enet_fit)
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
  qplot(alpha,
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
minlossplot(enet_fit)
.mlossp(enet_fit)
plot(enet_fit)
cvm <- sapply(enet_fit$modlist, function(mod) {
  mod$cvm[mod$lambda == mod[['lambda.1se']]]
})
min_cv <- which.min(cvm)

# from the above the optimal elasticnet model is at alpha 0.47, this is more towards ridge
# (alpha =0) and less towards lasso (alpha=1); but it kind of lands in the middle so it is the
# best of both worlds
# best value at alpha =0.47 where the cross validation loss is the lowest

# .47 is element 48

plot(enet_fit$modlist[[min_cv]])

e <- enet_fit$modlist[[min_cv]]

summary(e)
library("ggfortify")
# autoplot(, colour = 'blue', which = 1:6)

# # other candidates
# plot(enet_fit$modlist[[1]])  ## alphas zero, ridge model
# plot(enet_fit$modlist[[20]])
# plot(enet_fit$modlist[[28]]) ## best
# plot(enet_fit$modlist[[42]])
# plot(enet_fit$modlist[[66]])
# plot(enet_fit$modlist[[101]]) ## alphas zero, lasso  model

# coefficient matrix for the optimal elasticnet model using lambda.1se
coef(enet_fit, alpha = alpha_list[min_cv],
     s = enet_fit$modlist[[min_cv]]$lambda.1se) %>% round(3)

# ######################### Different method using carat #########################

library(caret)

model <- train(
  log(price) ~., data = wine_train %>% select(
    -variety_and_color
  ), method = "glmnet",
  trControl = trainControl("cv", number = 50),
  tuneLength = 60
)

model$bestTune


coef(model$finalModel, model$bestTune$lambda)

# Make predictions on the test data
x.train <- model.matrix(log(price) ~., wine_train%>%select(
  -ID,
  -country,
  -taster_avg_points,
  -taster_n_tweets,
  -taster_review_count,
  -title_word_count,
  -taster_twitter_lump,
  -variety
))[,-1]
predictions <- model %>% predict(wine_train)
# Model performance metrics
df <- data.frame(
  RMSE = RMSE(predictions, log(wine_train$price)),
  Rsquare = R2(predictions, log(wine_train$price))
)
#   RMSE   Rsquare
# 1 0.438321 0.5622834

########################### Framework to check preds ###########################

preds_train_DF <- data.frame(
  actual = log(wine_train$price),
  pred = predict(enet_fit, alpha = 0.26, lambda = lambda.min, wine_train) %>% round(3)
) %>% rename(actual = 1, pred = 2) %>% remove_rownames()
postResample(preds_train_DF$pred, preds_train_DF$actual)

library("caret")
preds_train_DF <- data.frame(
  actual=  -1 * (wine_train$price ^ (-.3)),
  pred = predict(enet_fit, alpha = 0.26, lambda = lambda.min, wine_train) %>% round(3)
) %>% rename(actual = 1, pred = 2) %>% remove_rownames() 
postResample(preds_train_DF$pred, preds_train_DF$actual)

preds_train_DF %>% ggplot(aes(x = actual, y = pred)) + geom_point()


##Plot of Actual Vs Preds

ggplot(mod1_df, aes(x = actual, y = pred)) + geom_point(color = "purple") +
  geom_abline(color = "red", linetype = "dashed")

mod1_df <- data.frame(pred  = preds_train_DF$pred,
                      actual = preds_train_DF$actual,
                      resids = preds_train_DF$actual-preds_train_DF$pred )

mod1_df


##Plots of Preds Vs Resids

ggplot(mod1_df,aes(x = pred, y = resids)) + 
  geom_point() + 
  geom_smooth() + 
  xlab("Fitted values")+ylab("Residuals")+
   geom_hline(yintercept=0, col="red", linetype="dashed")+ggtitle("Residual vs Fitted Plot")+theme_bw()

library('plotROC')
roc_plot_train <- ggplot(preds_train_DF, aes(m = pred, d = actual)) + geom_roc() + ggtitle("Training Data ROC curve") + style_roc()
roc_plot_train
