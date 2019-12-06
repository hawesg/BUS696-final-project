
# store rownames as columns
model.bootstrap.preds <- data.train %>% 
  rownames_to_column() %>% 
  mutate( rowname = as.numeric(rowname) ) %>% 
  select (
    price,
    points,
    #points.category,  # cannot use it along with points
    country,
    # province, #breaks bootstrap
    color,
    #variety,  #breaks bootstrap
    winery,
    taster.gender, 
    taster.avg_points,
    #variety_and_color,  #breaks bootstrap
    title.n_words,
    title.n_chars,
    title.sentement,
    title.has_accents,
    rowname
  ) 

glimpse(model.bootstrap.preds)

# bagging - bootstrapp aggregation
# Bootstrap model
# Created 30 samples with 1000 data points each (run into performance issues otherwise)
# Carefully selected variables, to allows bootstrapping
B <- 100      # number of bootstrap samples
num_b <- 500  # sample size of each bootstrap
boot_mods <- list() # store our bagging models
for(i in 1:B){
  boot_idx <- sample(1:nrow(data.train), 
                     size = num_b,
                     replace = FALSE)
  # fit a tree on each bootstrap sample
  data_slice = data.train %>% slice(boot_idx)
  
  # Log(price) bootstrap model ----
  boot_tree <- ctree(log(price)~ ., 
                     data = data_slice) 
  # store bootstraped model
  boot_mods[[i]] <- boot_tree
  # generate predictions for that bootstrap model
  preds_boot <- data.frame(
    preds_boot = predict(boot_tree),
    #resid =  data_slice$price - predict(boot_tree),
    rowname = boot_idx 
  )  
  
  
  # rename prediction to indicate which boot iteration it came from
  names(preds_boot)[1] <- paste("preds_boot",i,sep = "")
  
  # merge predictions to data.train dataset
  model.bootstrap.preds <- left_join(x = model.bootstrap.preds, y = preds_boot,
                                by = "rowname")
}
# 
# names(model.bootstrap.preds)
# ## Examine individual models, will need to spend more time here ----
# plot(boot_mods[[1]])
# plot(boot_mods[[2]])
# plot(boot_mods[[3]])
# plot(boot_mods[[4]])
# plot(boot_mods[[5]])
# plot(boot_mods[[6]])
# plot(boot_mods[[7]])
# plot(boot_mods[[8]])
# plot(boot_mods[[9]])
# plot(boot_mods[[10]])
# plot(boot_mods[[20]])
# plot(boot_mods[[30]])

# must convert factor into numeric, note that class "0" = 1, 
# and class "1" = 2, so we need to subtract 1 from every column
model.bootstrap.preds %<>% 
  mutate_if(is.factor, as.numeric) %>% 
  mutate_all(function(x){x - 1})

# calculate mean over all the bootstrap predictions
model.bootstrap.preds <- model.bootstrap.preds %>%
  mutate(preds_bag = select(., preds_boot1:preds_boot30) %>% 
                               rowMeans(na.rm = TRUE))


head(model.bootstrap.preds)

# plot bagged model -------------------------------------------------------
# ggplot(model.bootstrap.preds, aes(x = exp(preds_bag))) + geom_histogram()
# TODO FIX THIS SINCE exp no longer works
ggplot(model.bootstrap.preds, aes(x = preds_bag)) + geom_histogram()
summary(model.bootstrap.preds)

model.bootstrap.preds$preds_bag
