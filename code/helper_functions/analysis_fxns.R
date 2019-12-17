.tukey <- function(p){
  p <- p ^ (-.3)
  return(-p)
}


.save_model_metrics <- function(st, mdl, set, dep, ind, tun, pr, ac){
  
  tempPostResample<-postResample(pr, ac)
  metric.temp2 <- tibble(model = mdl, 
                         set=set, 
                         dependant = dep, 
                         independant=ind, 
                         tuning = tun, 
                         RMSE = tempPostResample["RMSE"], 
                         Rsquared = tempPostResample["Rsquared"], 
                         MAE = tempPostResample["MAE"])
  st <- model.stats %>% bind_cols(metric.temp2)
}




.model_summary <- function(preds, .actuals, model_title){
  tempPostResample<-postResample(p, a)
  summary_df <- data.frame(pred = preds,
                           actual = actuals,
                           resid = actuals-preds)
  ggplot(summary_df, aes(x = pred, y = resid)) + 
    geom_point() + 
    ggtitle(paste(model_title,"Residuals vs Predicted")) + 
    geom_smooth()
}


#################################### Models ####################################

#OlS Model (No Log Price) ----

model.ols.price <-
  lm(
    price ~
      country +
      variety +
      points +
      province +
      winery +
      color +
      designation +
      title.n_words +
      title.sentement +
      title.has_accents +
      taster.name +
      taster.gender,
    data = data.train,
  )

summary(model.ols.price)


# postResample.1<-postResample(predict(model.ols.price), data.train$price)
# postResample.2<-postResample(predict(model.ols.bkfit.log.tukey), .tukey(data.train$price))
# postResample.3<-postResample(model.logit.test.preds.df$pred, model.logit.test.preds.df$actual)
# postResample.4<-postResample(predict(model.enet, alpha = alpha_list[39], lambda = lambda.min, data.train), .tukey(data.train$price))

# postResample.1

# postResample.<-postResample(predict(model.ols.price), data.train$price)
# postResample.<-postResample(predict(model.ols.price), data.train$price)
model.stats <- tibble(model = "OLS", 
                      set="Train", 
                      dependant = "Price", 
                      independant="country + variety + points + province + winery + color + designation + title.n_words + title.sentement + title.has_accents + taster.name + taster.gender", 
                      tuning = "", 
                      RMSE = tempPostResample["RMSE"], 
                      Rsquared = tempPostResample["Rsquared"], 
                      MAE = tempPostResample["MAE"])

model.stats