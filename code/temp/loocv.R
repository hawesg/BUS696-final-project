library(tidyverse)
library(svMisc)
#install.packages("svMisc")
options(scipen = 50)
set.seed(1861)

wind_data_loocv <- wine_data_clean %>% select(-c("ID"))

train_wine_idx <- sample(1:nrow(wind_data_loocv), size = floor(0.75*nrow(wind_data_loocv)))
wine_train<- wind_data_loocv %>% slice(train_wine_idx)
wine_test <- wind_data_loocv %>% slice(-train_wine_idx)
                        
means <- data.frame(test = mean(wine_test$price), train = mean(wine_train$price))
t.test(wind_data_loocv$price, wind_data_loocv$price)


mods_LOOCV <- list()
preds_LOOCV <- NULL

for(i in 1:nrow(wine_train)){
  mod = lm(price ~ .,
           data = wine_train %>% slice(-i))
  preds_LOOCV[i] <- predict(mod, newdata =
                              slice(wine_train,i))
  mods_LOOCV[[i]] <- mod
}

save(mods_LOOCV, file = here::here("data","output","loocv","mods_LOOCV.RData"))
save(preds_LOOCV, file = here::here("data","output","loocv","preds_LOOCV.RData"))
save(wine_train, file = here::here("data","output","loocv","wine_train.RData"))
save(wine_test, file = here::here("data","output","loocv","pwine_test.RData"))

head(preds_LOOCV)

mod_insample <- lm(price ~ .,
                   data = wine_data_clean)

preds_DF <- data.frame(
  preds_LOOCV = preds_LOOCV,
  preds_insample = predict(mod_insample),
  true = red_train$quality
)
library('plotROC')
roc_plot_train <- ggplot(preds_DF, aes(m = preds_LOOCV, d = true)) + geom_roc() + ggtitle("Training Data ROC curve") + style_roc()
roc_plot_train


ggplot(preds_DF, aes(m = preds_LOOCV, d = true)) + geom_roc() + ggtitle("Training Data ROC curve"