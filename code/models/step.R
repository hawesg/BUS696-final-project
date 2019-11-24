###############################################################################-
#                                                                              #
# Purpose:       Forward and Backward Step Regression for selecting variables  #
#                                                                              #
# Author:        Garrett Hawes                                                 #
# Contact:       hawes102@mail.chapman.edu                                     #
# Client:        Garrett Hawes                                                 #
#                                                                              #
# Code created:  2019-11-22                                                    #
# Last updated:  2019-11-22                                                    #
# Source:        /Users/garretthawes/wine-project                              #
#                                                                              #
# Comment:       I have only started to play around with this some             #
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


############################## Forward Step Model ##############################
# ? sqrt
# + I(points^2)
# rm(fwd_fit)
fwd_fit <-
  regsubsets(
    log2(price) ~ . + I(sqrt(points)),
    data = wine_train %>% select(
      -ID,
      -country,
      -taster_avg_points_per,
      -taster_n_tweets_per,
      -taster_review_count_per,
      -title_word_count_per,
      -taster_twitter_lump
    ),
    nvmax = 24,
    method = "forward"
  )

names(wine_train)

############################# Backward Step Model ##############################

bkwd_fit <-
  regsubsets(
    log(price) ~ .,
    data = wine_train %>% select(
      -ID,
      -country,
      -taster_avg_points,
      -taster_n_tweets,
      -taster_review_count,
      -title_word_count,
      -taster_twitter_lump
    ),
    nvmax = 24,
    method = "backward"
  )

# Find linear dependencies

# names(wine_train)

reg.summary.fw <- summary(fwd_fit)


reg.summary.bk <- summary(bkwd_fit)

names(reg.summary.fw)

# rm(reg.summary.fw)


reg.summary.fw$rsq

# [1] 0.3921120 0.4209773 0.4395742 0.4530567 0.4681209 0.4762943 0.4828847 0.4882901 0.4928486 0.4969633 0.5006699 0.5034785 0.5064814 0.5091531 0.5111293
# [16] 0.5134877 0.5156343 0.5175077 0.5192894 0.5207811 0.5216558 0.5225177 0.5233079 0.5239052

reg.summary.bk$rsq
# [1] 0.3921120 0.4173112 0.4367624 0.4532179 0.4643926 0.4687823 0.4767820 0.4833105 0.4887640 0.4923295 0.4953244 0.4964609 0.4977307 0.4995556 0.5028151
# [16] 0.5064748 0.5087196 0.5107489 0.5138512 0.5164968 0.5190115 0.5200636 0.5209665 0.5229720

line_comment("Plots")

#################################### Plots #####################################

plot(bkwd_fit, scale = "bic")

plot(bkwd_fit, scale = "adjr2")


library(ggvis)
rsq <- as.data.frame(reg.summary.bk$rsq)
names(rsq) <- "R2"
rsq %>%
  ggvis(x =  ~ c(1:nrow(rsq)), y =  ~ R2) %>%
  layer_points(fill = ~ R2) %>%
  add_axis("y", title = "R2") %>%
  add_axis("x", title = "Number of variables")


par(mfrow = c(2, 2))
plot(reg.summary.bk$rss ,
     xlab = "Number of Variables ",
     ylab = "RSS",
     type = "l")
plot(reg.summary.bk$adjr2 ,
     xlab = "Number of Variables ",
     ylab = "Adjusted RSq",
     type = "l")
# which.max(reg.summary.bk$adjr2)
points(
  24,
  reg.summary.bk$adjr2[24],
  col = "red",
  cex = 2,
  pch = 20
)
plot(reg.summary.bk$cp ,
     xlab = "Number of Variables ",
     ylab = "Cp",
     type = 'l')
# which.min(reg.summary.bk$cp )
points(24,
       reg.summary.bk$cp [24],
       col = "red",
       cex = 2,
       pch = 20)
plot(reg.summary.bk$bic ,
     xlab = "Number of Variables ",
     ylab = "BIC",
     type = 'l')
# which.min(reg.summary.bk$bic )
points(24,
       reg.summary.bk$bic [24],
       col = "red",
       cex = 2,
       pch = 20)
print(reg.summary.bk)
view(reg.summary.bk)
par(mfrow = c(1, 1))
plot(bkwd_fit, scale = "bic")
coef(bkwd_fit , 24)
# (Intercept)                        points            point_catVery good          point_catOutstanding              variety_lumpRosé
# -8.54014926                    0.13228942                   -0.26868907                   -0.27829891                   -0.33775411
# variety_lumpOther          designation_lumpBrut       designation_lumpReserve designation_lumpSome Vineyard         designation_lumpOther
# -0.07055484                    0.35947875                    0.13501588                    0.15683734                    0.09111684
# taster_name_lumpOther                taster_genderM               color_lumpWhite         country_lumpAustralia            country_lumpFrance
# 0.24285201                   -0.15270510                   -0.24535373                    0.38777600                    0.25814728
# country_lumpItaly       country_lumpNew Zealand                country_lumpUS             country_lumpOther         province_lumpBurgundy
# 0.24474105                    0.38744602                    0.23166771                    0.36313575                    0.51842165
# province_lumpCalifornia           province_lumpOregon         province_lumpPiedmont           taster_n_tweets_per       taster_review_count_per
# 0.17920831                    0.17485765                    0.27751470                    0.26875706                    0.56358443
plot(bkwd_fit, scale = "Cp")


# Forward Fit
plot(fwd_fit, scale = "bic")

plot(fwd_fit, scale = "adjr2")

rsq <- as.data.frame(reg.summary.fw$rsq)
names(rsq) <- "R2"
rsq %>%
  ggvis(x =  ~ c(1:nrow(rsq)), y =  ~ R2) %>%
  layer_points(fill = ~ R2) %>%
  add_axis("y", title = "R2") %>%
  add_axis("x", title = "Number of variables")


par(mfrow = c(2, 2))
plot(reg.summary.fw$rss ,
     xlab = "Number of Variables ",
     ylab = "RSS",
     type = "l")
plot(reg.summary.fw$adjr2 ,
     xlab = "Number of Variables ",
     ylab = "Adjusted RSq",
     type = "l")
# which.max(reg.summary.fw$adjr2)
points(
  24,
  reg.summary.fw$adjr2[24],
  col = "red",
  cex = 2,
  pch = 20
)
plot(reg.summary.fw$cp ,
     xlab = "Number of Variables ",
     ylab = "Cp",
     type = 'l')
# which.min(reg.summary.fw$cp )
points(24,
       reg.summary.fw$cp [24],
       col = "red",
       cex = 2,
       pch = 20)
plot(reg.summary.fw$bic ,
     xlab = "Number of Variables ",
     ylab = "BIC",
     type = 'l')
# which.min(reg.summary.fw$bic )
points(24,
       reg.summary.fw$bic [24],
       col = "red",
       cex = 2,
       pch = 20)
print(reg.summary.fw)
view(reg.summary.fw)
par(mfrow = c(1, 1))
plot(fwd_fit, scale = "bic")

################################### Results ####################################

coef(fwd_fit , 24)
# (Intercept)                points    point_catVery good  point_catOutstanding      variety_lumpRosé  designation_lumpBrut        taster_genderM
# -8.3698279             0.1352688            -0.2724367            -0.2759206            -0.2688027             0.3317582            -0.1361889
# color_lumpWhite     country_lumpItaly        country_lumpUS province_lumpBurgundy
# -0.2566484             0.1613557             0.1885719             0.6451918
plot(bkwd_fit, scale = "Cp")
