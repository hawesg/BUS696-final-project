# ### Probably irrelevant because we are looking to model
# #seq(0, 3000, by = 25)
# # stats <-
# #   wine_data %>% group_by(pr = cut(
# #     price,
# #     breaks = c(0, 10, 25, 50, 100, 200, 500, 1000, 3500),
# #     dig.lab = 5
# #   )) %>% summarize(
# #     count = n(),
# #     min = min(points),
# #     max = max(points),
# #     avg = mean(points),
# #     sd = sd(points)
# #   )
# # stats
# # 
# # 
# #install.packages("summarytools")
# library(summarytools)
# summarytools::descr(wine_data_clean)
# # Descriptive Statistics  
# # wine_data_clean  
# # N: 91500  
# # 
# # ID     points      price   taster_avg_points   taster_review_count
# # ----------------- ---------- ---------- ---------- ------------------- ---------------------
# # Mean   45750.50      88.61      35.29               88.61              11256.58
# # Std.Dev   26413.92       2.97      43.29                0.95               5946.77
# # Min       1.00      80.00       4.00               86.61                  6.00
# # Q1   22875.50      87.00      17.00               88.54               6237.00
# # Median   45750.50      88.00      25.00               88.63               9507.00
# # Q3   68625.50      91.00      42.00               89.09              14944.00
# # Max   91500.00     100.00    3300.00               90.61              20172.00
# # MAD   33914.47       2.97      14.83                0.68               7911.15
# # IQR   45749.50       4.00      25.00                0.55               8707.00
# # CV       0.58       0.03       1.23                0.01                  0.53
# # Skewness       0.00      -0.01      19.23               -0.45                  0.24
# # SE.Skewness       0.01       0.01       0.01                0.01                  0.01
# # Kurtosis      -1.20      -0.24     872.76                0.04                 -1.14
# # N.Valid   91500.00   91500.00   91500.00            91500.00              91500.00
# # Pct.Valid     100.00     100.00     100.00              100.00                100.00
# # 
# # Table: Table continues below
# # 
# # 
# # 
# # title_length
# # ----------------- --------------
# #   Mean          52.75
# # Std.Dev          13.74
# # Min          12.00
# # Q1          43.00
# # Median          52.00
# # Q3          61.00
# # Max         136.00
# # MAD          13.34
# # IQR          18.00
# # CV           0.26
# # Skewness           0.54
# # SE.Skewness           0.01
# # Kurtosis           0.56
# # N.Valid       91500.00
# # Pct.Valid         100.00
# 
# 
# 
# summarytools::descr(wine_data_clean, transpose = TRUE)
# 
# dfSummary(wine_data_clean)



# Forward Step Model ------------------------------------------------------


fwd_fit <- 
  regsubsets(price ~ ., 
             data = wine_data_clean%>%select(-ID, -country, -taster_avg_points, -taster_n_tweets, -taster_review_count, -title_word_count),
             nvmax = 24,
             method = "forward")

bkwd_fit <- 
  regsubsets(price ~ ., 
             data = wine_data_clean%>%select(-ID, -country, -taster_avg_points, -taster_n_tweets, -taster_review_count, -title_word_count),
             nvmax = 24,
             method = "backward")

names(wine_data_clean)

reg.summary.fw = summary(fwd_fit)
reg.summary = summary(bkwd_fit)

names(reg.summary)

reg.summary.fw$rsq
reg.summary$rsq

plot(fwd_fit,scale="bic")

plot(fwd_fit, scale = "adjr2")

library(ggvis)
rsq <- as.data.frame(reg.summary$rsq)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "Number of variables")
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
#which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
#which.min(reg.summary$cp )
points(10,reg.summary$cp [10],col="red",cex=2,pch=20)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
# which.min(reg.summary$bic )
points(6,reg.summary$bic [6],col="red",cex=2,pch=20)
print(reg.summary)
view(reg.summary)
plot(bkwd_fit,scale="bic")
coef(bkwd_fit ,10)
plot(bkwd_fit, scale = "Cp")


# Load most commonly used libraries

# need to include: caret + lattice + glmnet + glmnetUtils + coefplot

# Elasticnet --------------------------------------------------------------


names(wine_data_clean)


set.seed(1861)
trainSize <- 0.30
train_idx <- sample(1:nrow(wine_data_clean), size = floor(nrow(wine_data_clean) * trainSize))

wine_data_clean_rm_model <- wine_data_clean %>%
  select (
    price,
    points,
    point_cat,   
    variety_lump,  
    country_lump,
    province_lump,   
    taster_name_lump,
    # taster_twitter_lump,  #not helpful
    taster_gender,
    #taster_avg_points,
    taster_avg_points_per,   # no added value
    #taster_review_count,
    taster_review_count_per,  # no added value
    #taster_n_tweets,
    taster_n_tweets_per,
    designation_lump,
    color_lump,
    #title_word_count,
    # title_word_count_per, # no added value
    #title_sentement # no added value
  ) 

ElasticNetModel_train <- wine_data_clean_rm_model %>% slice(train_idx)
ElasticNetModel_test <- wine_data_clean_rm_model %>% slice(-train_idx)


# remove n/a values
ElasticNetModel_train <- ElasticNetModel_train[apply(is.na(ElasticNetModel_train),1,sum)==0,]
ElasticNetModel_test <- ElasticNetModel_test[apply(is.na(ElasticNetModel_test),1,sum)==0,]

#using length 100
alpha_list <- seq(0,1,len = 101)
alpha_list

#get the model
conflict_prefer("mutate", "dplyr")
enet_fit <- cva.glmnet(price ~ ., 
                       data = ElasticNetModel_train, 
                       alpha = alpha_list)

print(enet_fit)


### minlossplot
minlossplot(enet_fit)
?minlossplot
plot(enet_fit)

#  from the above the optimal elasticnet model is at alpha 0.28, this is more towards ridge (alpha =0) and less towards lasso (alpha=1); but it kind of lands in the middle so it is the best of both worlds
# best value at alpha =0.28 where the cross validation loss is the lowest

plot(enet_fit$modlist[[28]])

# other candidates
plot(enet_fit$modlist[[1]])  ## alphas zero, ridge model
plot(enet_fit$modlist[[20]])
plot(enet_fit$modlist[[28]]) ## best
plot(enet_fit$modlist[[42]])
plot(enet_fit$modlist[[66]])
plot(enet_fit$modlist[[101]]) ## alphas zero, lasso  model


# coefficient matrix for the optimal elasticnet model using lambda.1se
coef(enet_fit, alpha = 0.28, 
     s = enet_fit$modlist[[28]]$lambda.1se)



