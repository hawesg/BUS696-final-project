as reference if need be use: code/temp/markdown/bootstrap_eval.RMD

====================================================================
Slide 1: Modelling log(price) using decision-tree via Bootstrap technique

* # of Bootstrap sample to 100
* size of each bootstrap = 500 to ensure adequate data
* ctree model selected 

{R - sample}
	B <- 100      # number of bootstrap samples
	num_b <- 500  # sample size of each bootstrap
	boot_mods <- list() # store our bagging models
	for(i in 1:B){
	  boot_idx <- sample(1:nrow(wine_train), 
						 size = num_b,
						 replace = FALSE)
	  # fit a tree on each bootstrap sample
	  data_slice = wine_train %>% slice(boot_idx)
	  
	  # Log(price) bootstrap model ----
	  boot_tree <- ctree(log(price) ~ ., 
						 data = data_slice 
	  ) .....
{/R - sample}

====================================================================

Slide 2: Example of one of the decision trees

{R}
	plot(boot_mods[[5]])
{/R}

====================================================================

Slide 3: Actual Price vs  Predicted Price

{left part of slide}
	# log(price) distribution of the wine train dataset
	{R}
		ggplot(wine_train, aes(x = log(price))) + xlim(0, 10) + geom_histogram(binwidth=0.1)
	{/R}
{/left part of slide}
{right part of slide}
	# Bootstrap model: distribution of the predicted values
	{R}
		ggplot(wine_train_preds, aes(x = preds_bag)) + xlim(0, 10) + geom_histogram(binwidth=0.1)
	{/R}
{/right part of slide}

====================================================================

Slide 4: Summary of Boot strap
* variations of bootstrap predictions appear to match training dataset
* bootstrap predictions have a bias, a tendency to undervalue wine
* most root and decision nodes use points for decision to split, then taster.avg_points and variety are used