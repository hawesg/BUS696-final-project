###############################################################################-

################################## Libraries ###################################
#                                                                              #
#  Omit this if you want to just load them manually                            #
#                                                                              #
###############################################################################-
#
source("code/libraries.R")
library(caret)

wine_data_to_be_standardized <-
  wine_data_clean %>% select(
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
  wine_data_clean %>% select(
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
wine_data_standardized <- bind_cols(wine_data_not_to_be_standardized, transformed)
str(wine_data_standardized)
library("skimr")
skim(wine_data_standardized)

library(glmnet)
library(glmnetUtils)

alpha_list <- seq(0, 1, len = 101)
alpha_list
set.seed(1861)
options(scipen = 50)
TRAIN.PERCENT <- 0.75 
inTrainSetIndex <- createDataPartition(y = wine_data_standardized$price, p=TRAIN.PERCENT, list=FALSE, groups=5)
data.train   <- wine_data_standardized[ inTrainSetIndex, ]
data.test <- wine_data_standardized[-inTrainSetIndex, ]

enet_fit <- cva.glmnet(
  -1 * price ^ (-.3) ~ . - variety_and_color,
  data = data.train,
  trace.it=1
  ,
  alpha = alpha_list
)


# model <- train(
#   log(price) ~., data = wine_train %>% select(
#     -variety_and_color
#   ), method = "glmnet",
#   trControl = trainControl("cv", number = 50),
#   tuneLength = 60
# )


print(enet_fit)

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
       ylab="CV loss", 
       xlab = paste0("alpha (min cv - ", alpha[min_cv], ")"),
       ...)
  title("CV loss v alpha Elastic Net Model")
  points(
    x = alpha[min_cv],
    y = cvm[min_cv],
    pch = 20,
    col = "red",
    bg = "yellow",
    cex = 0.90
  )
  invisible(x)
}


### minlossplot
.mlossp(enet_fit, cv.type="1se")
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


preds_train_DF <- data.frame(
  actual=  -1 * (data.train$price ^ (-.3)),
  pred = predict(enet_fit, alpha = 1, lambda = lambda.min, data.train) %>% round(3)
) %>% rename(actual = 1, pred = 2) %>% remove_rownames() 
postResample(preds_train_DF$pred, preds_train_DF$actual)


preds_test_DF <- data.frame(
  actual=  -1 * (data.test$price ^ (-.3)),
  pred = predict(enet_fit, alpha = 1, lambda = lambda.min, newdata = data.test) %>% round(3)
) %>% rename(actual = 1, pred = 2) %>% remove_rownames() 
postResample(preds_test_DF$pred, preds_test_DF$actual)

# ggplot(preds_train_DF, aes(x = actual, y = pred)) + geom_point(color = "purple") +
#   geom_abline(color = "red", linetype = "dashed")


mod1_df <- data.frame(pred  = preds_train_DF$pred,
                      actual = preds_train_DF$actual,
                      resids = preds_train_DF$actual-preds_train_DF$pred )
mod1_test_df <- data.frame(pred  = preds_test_DF$pred,
                      actual = preds_test_DF$actual,
                      resids = preds_test_DF$actual-preds_test_DF$pred )


preds_price_train_df <- preds_test_DF %>% mutate(
  actual = (-1 / actual) ^ (10/3),
  pred = (-1 / pred) ^ (10/3)
)

mod1_price_df <- data.frame(pred  = preds_price_train_df$pred,
                            actual = preds_price_train_df$actual,
                            resids = preds_price_train_df$actual-preds_price_train_df$pred )
head(mod1_price_df)


# ggplot(mod1_price_df,aes(x = actual, y = pred)) + 
#   geom_jitter() + 
#   xlab("Actual")+ylab("Predicted")+ggtitle("Predicted vs Actual Plot")+theme_bw()
# 
# 
# mod1_price_df%>% View()
# ##Plots of Preds Vs Resids

ggplot(mod1_df,aes(x = pred, y = resids)) + 
  geom_point() + 
  geom_smooth() + 
  xlab("Fitted values")+ylab("Residuals")+
  geom_hline(yintercept=0, col="red", linetype="dashed")+ggtitle("Residual vs Fitted Plot")+theme_bw()


ggplot(mod1_price_df,aes(x = pred, y = resids)) + 
  geom_point() + 
  geom_smooth() + 
  xlab("Fitted values")+ylab("Residuals")+
  geom_hline(yintercept=0, col="red", linetype="dashed")+ggtitle("Residual vs Fitted Plot Test Set")+theme_bw()



ggplot(mod1_test_df,aes(x = pred, y = resids)) + 
  geom_point() + 
  geom_smooth() + 
  xlab("Fitted values")+ylab("Residuals")+
  geom_hline(yintercept=0, col="red", linetype="dashed")+ggtitle("Residual vs Fitted Plot Test Set")+theme_bw()



