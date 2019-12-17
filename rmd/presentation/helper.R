# commentr::line_comment("Residuals Plot")

model.comparison <- data.table()

############################# Map Plot Helper Info #############################

world_map <- map_data("world")
world_map <-
  world_map %>% dplyr::mutate(region = ifelse(region == "USA", "US", region))
world_map <- world_map %>% dplyr::mutate(country = region)

wine_map_DF <-
  wine_data_clean  %>% dplyr::mutate(country = as.character(country)) %>%
  dplyr::filter (country != "England" &
                   country != "US-France" &
                   country != "")

# Generate Summary
wmap <-
  wine_data_clean %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(
    point_min = min(points, na.rm = TRUE),
    point_avg = mean(points, na.rm = TRUE),
    point_max = max(points, na.rm = TRUE),
    price_min = min(price, na.rm = TRUE),
    price_avg = mean(price, na.rm = TRUE),
    price_max = max(price, na.rm = TRUE),
    count = n()
  ) %>%
  dplyr::select(
    c(
      "country",
      "point_min",
      "point_avg",
      "point_max",
      "price_min",
      "price_avg",
      "price_max",
      "count"
    )
  )
# Create data frame with bounderies and values
wine_country_map <- right_join(wmap, world_map, by = "country")

#scale_fill_viridis_c(option = "C", limits = c(80, 100))

.map_from_attribute <- function(att, title) {
  return(
    ggplot(wine_country_map, aes_string(map_id = "country", fill = att)) +
      geom_map(map = wine_country_map,  color = '#fdf6e3') +
      expand_limits(x = wine_country_map$long, y = wine_country_map$lat) +
      scale_fill_viridis_c(option = "C") + theme_map(base_size = 16) + ggtitle(title) +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()
      ) +
      theme(panel.background = element_rect('#fdf6e3', '#fdf6e3', 0, 'solid')) +
      theme(plot.background = element_rect(fill = "#fdf6e3")) +
      theme(
        legend.background = element_rect(
          fill = "#fdf6e3",
          size = 0.5,
          linetype = "solid"
        )
      )
  )
}

.map_from_attribute("point_avg", "Average Points")

################################## ggmarginal ##################################

plot.ggmarginal.price.p <-
  ggplot(wine_data_clean, aes(points, price, color = points.category)) +
  geom_point() +
  labs(
    color = "Rating Category",
    title = "Price vs Rating",
    x = "Points",
    y = "Price"
  ) +
  theme_clean(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  theme(legend.key = element_blank())

plot.ggmarginal.price.p1 <-
  ggMarginal(
    plot.ggmarginal.price.p,
    type = "histogram",
    fill = "blue",
    groupColour = TRUE
  )
plot.ggmarginal.price.p1

plot.ggmarginal.log.p <-
  ggplot(
    wine_data_clean %>% select(points, price, point_cat),
    aes(points, price, color = point_cat)
  ) +
  geom_point() +
  theme_clean(base_size = 16) +
  scale_y_log10() +
  labs(
    color = "Rating Category",
    title = "Price vs Rating",
    caption = "Price is scaled to log 10",
    x = "Points",
    y = "Price"
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
plot.ggmarginal.log.p1 <-
  ggMarginal(
    plot.ggmarginal.log.p,
    type = "histogram",
    fill = "blue",
    groupColour = TRUE
  )
plot.ggmarginal.log.p1

############################### Price by country ###############################

plot.price_by_country <-
  ggplot(wine_data_clean,
         aes(x = country, y = price, color = points.category)) +
  scale_y_log10() +
  geom_jitter(alpha = 1 / 2) +
  coord_flip() +
  theme_solarized() +
  labs(
    color = "Rating Category",
    title = "Price by Country",
    caption = "Price is scaled to log 10",
    x = "Country",
    y = "Price"
  )  +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    legend.key = element_rect(fill = '#fdf6e3'),
    legend.box.background = element_rect(colour = "#fdf6e3")
  ) + theme(legend.position = "bottom")

plot.price_by_country


prices <- wine_data_clean$price

################################## OLS - Log ###################################

library("leaps")
ols.log <- regsubsets(
  log(price) ~
    country +
    winery +
    color +
    variety +
    designation +
    title.has_accents +
    taster.name +
    points +
    title.n_words +
    title.sentement,
  data = data.train,
  method = "backward",
  nvmax = 10
)

################################# Elastic Net ##################################

library("glmnet")
library("glmnetUtils")

model = model.enet
traindata = data.train
testdata = data.test


plot(
  boot_mods[[1]],
  gp = gpar(fontsize = 6, bg = '#fdf6e3'),
  inner_panel = node_inner,
  ip_args = list(abbreviate = FALSE, id = TRUE)
)

#################### Working with lowering the size of svg #####################


# data.marginal.plot_df <-
#   wine_data_clean %>% select(points, price, point_cat) %>% unique()
# plot.marginal.plot_df <-
#   ggplot(data.marginal.plot_df, aes(points, price, color = point_cat)) +
#   geom_point() +
#   labs(
#     color = "Rating Category",
#     title = "Price vs Rating",
#     x = "Points",
#     y = "Price"
#   ) +
#   theme_solarized_2(base_size = 16) +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom")
#
# plot.marginal.plot_df
#
# plot.marginal.plot.1 <-
#   ggMarginal(
#     plot.marginal.plot_df,
#     type = "histogram",
#     fill = "blue",
#     groupColour = TRUE
#   )




########################### Random Forest Min Depth ############################

plot.bootstrap.min_depth_distribution <-
  plot_min_depth_distribution(rf_fit, mean_sample = "relevant_trees", k = 15)
plot.bootstrap.min_depth_distribution + theme_solarized() + theme(legend.position = "bottom")

############################# Plot for bootstap v1 #############################

ggplot(wine_train_preds) +
  xlim(0, 10) +
  labs(
    title = "Actual vs Predicted Distribution",
    caption = "Red line is mean of actual",
    x = "Log of price",
    y = "Count"
  ) +
  geom_histogram(
    aes(x = log(price), color = "Actual"),
    binwidth = 0.15,
    alpha = .3,
    fill = "#00AFBB"
  ) +
  geom_histogram(
    aes(x = preds_bag, color = "Preds Bag"),
    binwidth = 0.15,
    alpha = .2,
    fill = "#E7B800"
  ) +
  geom_vline(
    aes(xintercept = mean(log(price))),
    color = "#FC4E07",
    linetype = "dashed",
    size = 1
  ) +
  theme_solarized(base_size = 16) +
  scale_color_manual(
    name = "Data",
    breaks = c("Actual", "Preds Bag"),
    values = c("Actual" = "#00AFBB", "Preds Bag" = "#E7B800")
  )

######################## Standardized data for ggpairs #########################

wine_data_standardized <- data.train %>% bind_rows(data.test)

wine_data_standardized_tukey <-
  wine_data_standardized %>% mutate(price = .tukey(price))

dput(names(wine_data_standardized_tukey %>% select_if(is.numeric)))

wine_data_tukey <- data.train %>% mutate(price = .tukey(price))

ggpairs.variables <-
  c("price", "points", "title.n_chars", "title.has_accents")

plot.ggpairs <-
  ggpairs(wine_data_standardized_tukey,
          columns = ggpairs.variables,
          ggplot2::aes(color = color, alpha = .2))

############################# Var Imporance plots ##############################

imp <- varImpPlot(rf_fit)# let's save the varImp objec
head(imp)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL
names(imp) <- c("X.IncMSE", "IncNodePurity", "varnames")

ggplot(imp, aes(
  x = reorder(varnames, IncNodePurity),
  y = IncNodePurity,
  color = as.factor(varnames)
)) +
  geom_point(size = 8) +
  ylab("Increase in Node Purity") +
  xlab("") +
  ggtitle("Type 2") +
  coord_flip() +
  theme_solarized_2(base_size = 16) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(imp, aes(
  x = reorder(varnames, X.IncMSE),
  y = X.IncMSE,
  color = as.factor(varnames)
)) +
  #geom_point(size=8) +
  ylab("Percent Increase MSE") +
  xlab("") +
  ggtitle("Type 1") +
  coord_flip() +
  theme_solarized(base_size = 16) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_emoji("wine_glass")

# TODO: Get this nice emoji based plot to work again.

################################ Helper Functions ################################

#TODO Combine these into one

.resid_plots <- function(model, traindata, testdata)
{
  temp.train_df <- data.frame(
    actual =  .tukey(traindata$price),
    pred = predict(model, data = traindata) %>% round(4)
  ) %>% rename(actual = 1, pred = 2) %>% remove_rownames() %>%  mutate(resid = actual - pred)
  
  temp.test_df <- data.frame(
    actual =  .tukey(testdata$price),
    pred = predict(model, newdata = testdata) %>% round(4)
  ) %>% rename(actual = 1, pred = 2) %>% remove_rownames() %>%  mutate(resid = actual - pred)
  
  temp.ylim.low <- min(temp.train_df$resid, temp.test_df$resid)
  temp.ylim.high <- max(temp.train_df$resid, temp.test_df$resid)
  temp.xlim.low <- min(temp.train_df$pred, temp.test_df$pred)
  temp.xlim.high <- max(temp.train_df$pred, temp.test_df$pred)
  
  
  temp.pr.train <- postResample(temp.train_df$pred,
                                temp.train_df$actual) %>% round(4)
  
  temp.pr.test <- postResample(temp.test_df$pred,
                               temp.test_df$actual) %>% round(4)
  
  
  temp.metrics <-
    data.table(
      r2_train = temp.pr.train['Rsquared'],
      r2_test = temp.pr.test['Rsquared'],
      RMSE_train = temp.pr.train['RMSE'],
      RMSE_test = temp.pr.test['RMSE'],
      MAE_train = temp.pr.train['MAE'],
      MAE_test = temp.pr.test['MAE']
    )
  
  model.comparison <<-
    model.comparison %>% bind_rows(temp.metrics)
  
  temp.plot.train <-
    ggplot(temp.train_df, aes(x = pred, y = resid)) +
    geom_point() +
    geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
    xlim(temp.xlim.low, temp.xlim.high) +
    ylim(temp.ylim.low, temp.ylim.high) +
    labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted Values",
      subtitle = "Training Set"
    ) +
    geom_hline(yintercept = 0,
               col = "red",
               linetype = "dashed") + theme_solarized(base_size = 16) +
    ggplot2::annotate(
      "text",
      temp.xlim.high,
      temp.ylim.high,
      hjust = 1,
      vjust = 1,
      color = "red",
      size = 6,
      label =
        paste(
          "R-squared",
          format(temp.pr.train['Rsquared'], nsmall = 4),
          "| RMSE: ",
          format(temp.pr.train['RMSE'], nsmall = 4),
          "| MAE: ",
          format(temp.pr.train['MAE'], nsmall = 4)
        )
    )
  
  temp.plot.test <-
    ggplot(temp.test_df, aes(x = pred, y = resid)) +
    geom_point() +
    geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
    xlim(temp.xlim.low, temp.xlim.high) +
    ylim(temp.ylim.low, temp.ylim.high) +
    labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted Values",
      subtitle = "Test Set"
    ) +
    geom_hline(yintercept = 0,
               col = "red",
               linetype = "dashed") + theme_solarized(base_size = 16) +
    ggplot2::annotate(
      "text",
      temp.xlim.high,
      temp.ylim.high,
      hjust = 1,
      vjust = 1,
      color = "red",
      size = 6,
      label =
        paste(
          "R-squared",
          format(temp.pr.test['Rsquared'], nsmall = 4),
          "| RMSE: ",
          format(temp.pr.test['RMSE'], nsmall = 4),
          "| MAE: ",
          format(temp.pr.test['MAE'], nsmall = 4)
        )
    )
  
  temp.plots <- list(temp.plot.train, temp.plot.test)
  
  return(temp.plots)
}

.resid_plots.log <- function(model, traindata, testdata)
{
  temp.train_df <- data.frame(
    actual =  log(traindata$price),
    pred = predict(model, data = traindata) %>% round(4)
  ) %>% rename(actual = 1, pred = 2) %>% remove_rownames() %>%  mutate(resid = actual - pred)
  
  temp.test_df <- data.frame(
    actual =  log(testdata$price),
    pred = predict(model, newdata = testdata) %>% round(4)
  ) %>% rename(actual = 1, pred = 2) %>% remove_rownames() %>%  mutate(resid = actual - pred)
  
  temp.ylim.low <- min(temp.train_df$resid, temp.test_df$resid)
  temp.ylim.high <- max(temp.train_df$resid, temp.test_df$resid)
  temp.xlim.low <- min(temp.train_df$pred, temp.test_df$pred)
  temp.xlim.high <- max(temp.train_df$pred, temp.test_df$pred)
  
  
  temp.pr.train <- postResample(temp.train_df$pred,
                                temp.train_df$actual) %>% round(4)
  
  temp.pr.test <- postResample(temp.test_df$pred,
                               temp.test_df$actual) %>% round(4)
  
  temp.metrics <-
    data.table(
      r2_train = temp.pr.train['Rsquared'],
      r2_test = temp.pr.test['Rsquared'],
      RMSE_train = temp.pr.train['RMSE'],
      RMSE_test = temp.pr.test['RMSE'],
      MAE_train = temp.pr.train['MAE'],
      MAE_test = temp.pr.test['MAE']
    )
  
  model.comparison <<-
    model.comparison %>% bind_rows(temp.metrics)
  
  temp.plot.train <-
    ggplot(temp.train_df, aes(x = pred, y = resid)) +
    geom_point() +
    geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
    xlim(temp.xlim.low, temp.xlim.high) +
    ylim(temp.ylim.low, temp.ylim.high) +
    labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted Values",
      subtitle = "Training Set"
    ) +
    geom_hline(yintercept = 0,
               col = "red",
               linetype = "dashed") + theme_solarized(base_size = 16) +
    ggplot2::annotate(
      "text",
      temp.xlim.high,
      temp.ylim.high,
      hjust = 1,
      vjust = 1,
      color = "red",
      size = 6,
      label =
        paste(
          "R-squared",
          format(temp.pr.train['Rsquared'], nsmall = 4),
          "| RMSE: ",
          format(temp.pr.train['RMSE'], nsmall = 4),
          "| MAE: ",
          format(temp.pr.train['MAE'], nsmall = 4)
        )
    )
  
  temp.plot.test <-
    ggplot(temp.test_df, aes(x = pred, y = resid)) +
    geom_point() +
    geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
    xlim(temp.xlim.low, temp.xlim.high) +
    ylim(temp.ylim.low, temp.ylim.high) +
    labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted Values",
      subtitle = "Test Set"
    ) +
    geom_hline(yintercept = 0,
               col = "red",
               linetype = "dashed") + theme_solarized(base_size = 16) +
    ggplot2::annotate(
      "text",
      temp.xlim.high,
      temp.ylim.high,
      hjust = 1,
      vjust = 1,
      color = "red",
      size = 6,
      label =
        paste(
          "R-squared",
          format(temp.pr.test['Rsquared'], nsmall = 4),
          "| RMSE: ",
          format(temp.pr.test['RMSE'], nsmall = 4),
          "| MAE: ",
          format(temp.pr.test['MAE'], nsmall = 4)
        )
    )
  
  temp.plots <- list(temp.plot.train, temp.plot.test)
  
  return(temp.plots)
}

.resid_plots.enet <- function(model, traindata, testdata)
{
  temp.train_df <- data.frame(
    actual =  .tukey(traindata$price),
    pred = predict(
      model.enet,
      alpha = alpha_list[min_cv],
      lambda = lambda.min,
      data.train
    ) %>% round(4)
  ) %>% rename(actual = 1, pred = 2) %>% remove_rownames() %>%  mutate(resid = actual - pred)
  
  temp.test_df <- data.frame(
    actual =  .tukey(testdata$price),
    pred = predict(
      model.enet,
      alpha = alpha_list[min_cv],
      lambda = lambda.min,
      newdata = data.test
    ) %>% round(4)
  ) %>% rename(actual = 1, pred = 2) %>% remove_rownames() %>%  mutate(resid = actual - pred)
  
  temp.ylim.low <- min(temp.train_df$resid, temp.test_df$resid)
  temp.ylim.high <- max(temp.train_df$resid, temp.test_df$resid)
  temp.xlim.low <- min(temp.train_df$pred, temp.test_df$pred)
  temp.xlim.high <- max(temp.train_df$pred, temp.test_df$pred)
  
  
  temp.pr.train <- postResample(temp.train_df$pred,
                                temp.train_df$actual) %>% round(4)
  
  temp.pr.test <- postResample(temp.test_df$pred,
                               temp.test_df$actual) %>% round(4)
  
  temp.metrics <-
    data.table(
      r2_train = temp.pr.train['Rsquared'],
      r2_test = temp.pr.test['Rsquared'],
      RMSE_train = temp.pr.train['RMSE'],
      RMSE_test = temp.pr.test['RMSE'],
      MAE_train = temp.pr.train['MAE'],
      MAE_test = temp.pr.test['MAE']
    )
  
  model.comparison <<-
    model.comparison %>% bind_rows(temp.metrics)
  
  temp.enet.alpha <- alpha_list[min_cv]
  
  temp.plot.train <-
    ggplot(temp.train_df, aes(x = pred, y = resid)) +
    geom_point() +
    geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
    xlim(temp.xlim.low, temp.xlim.high) +
    ylim(temp.ylim.low, temp.ylim.high) +
    labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted Values",
      subtitle = "Training Set",
      caption = paste(
        expression(lambda),
        ": 1se | ",
        expression(alpha),
        ": ",
        plot.enet.alpha
      )
    ) +
    geom_hline(yintercept = 0,
               col = "red",
               linetype = "dashed") + theme_solarized(base_size = 16) +
    ggplot2::annotate(
      "text",
      temp.xlim.high,
      temp.ylim.high,
      hjust = 1,
      vjust = 1,
      color = "red",
      size = 6,
      label =
        paste(
          "R-squared",
          format(temp.pr.train['Rsquared'], nsmall = 4),
          "| RMSE: ",
          format(temp.pr.train['RMSE'], nsmall = 4),
          "| MAE: ",
          format(temp.pr.train['MAE'], nsmall = 4)
        )
    )
  
  temp.plot.test <-
    ggplot(temp.test_df, aes(x = pred, y = resid)) +
    geom_point() +
    geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
    xlim(temp.xlim.low, temp.xlim.high) +
    ylim(temp.ylim.low, temp.ylim.high) +
    labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Residuals vs Fitted Values",
      subtitle = "Test Set",
      caption = paste(
        expression(lambda),
        ": 1se | ",
        expression(alpha),
        ": ",
        plot.enet.alpha
      )
    ) +
    geom_hline(yintercept = 0,
               col = "red",
               linetype = "dashed") + theme_solarized(base_size = 16) +
    ggplot2::annotate(
      "text",
      temp.xlim.high,
      temp.ylim.high,
      hjust = 1,
      vjust = 1,
      color = "red",
      size = 6,
      label =
        paste(
          "R-squared",
          format(temp.pr.test['Rsquared'], nsmall = 4),
          "| RMSE: ",
          format(temp.pr.test['RMSE'], nsmall = 4),
          "| MAE: ",
          format(temp.pr.test['MAE'], nsmall = 4)
        )
    )
  
  temp.plots <- list(temp.plot.train, temp.plot.test)
  
  return(temp.plots)
}




