###############################################################################-
#                                                                              #
# Purpose:      Leave one out Cross Validation                                 #
#                                                                              #
# Author:        Garrett Hawes                                                 #
# Contact:       hawes102@mail.chapman.edu                                     #
# Client:        Garrett Hawes                                                 #
#                                                                              #
# Code created:  2019-11-22                                                    #
# Last updated:  2019-11-22                                                    #
# Source:        /Users/garretthawes/wine-project                              #
#                                                                              #
# Comment:       Very rough idea, will likely leave out since it is            #
#                computationally expensive                                     #
#                                                                              #
###############################################################################-


###############################################################################-
#                                                                              #
#  Copy this stuff into the top of any script you are working on for modeling  #
#                                                                              #
###############################################################################-

# rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

load(here::here("data","output","clean_wine.RData"))

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

# wind_data_loocv <- wine_data_clean %>% select(-c("ID"))
                        
# means <- data.frame(test = mean(wine_test$price), train = mean(wine_train$price))
# t.test(wind_data_loocv$price, wind_data_loocv$price)

mods_LOOCV <- list()
preds_LOOCV <- NULL

names(wine_train_sub)
wine_train_sub <- wine_train%>%sample_n(100)%>%select(-c("ID", "country", "taster_name_lump", "title_word_count", "taster_n_tweets", "taster_review_count", "taster_avg_points" ))
names(wine_train_sub)
for(i in 1:nrow(wine_train_sub)){
  mod = lm(log(price) ~ .,
           data = wine_train_sub %>% slice(-i))
  preds_LOOCV[i] <- predict(mod, newdata =
                              slice(wine_train_sub,i))
  mods_LOOCV[[i]] <- mod
}

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



