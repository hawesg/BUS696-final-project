as reference if need be use: code/temp/markdown/R_Forest_eval.html

====================================================================
Slide 1: Modelling log(price) using R Forest

* using reduced data size -- lack of computational horsepower
* purpose: modelling log(price) that generates a set of bootstrapp trees (i.e. forests) and combine results via average regresion 

====================================================================

Slide 2: Summary of Results

* Number of trees: 500
* Mean of squared residuals: 0.1756748
* Var explained: 61.2

====================================================================

Slide 3: Gini Coefficient

* %IncMSE - higher means more important 
* IncNodePurity - Total decrease in node impurities from splitting on the variable, averaged over all trees. 
{left part of slide}
	# R Forest Fit using %IncMSE
	{R}
		varImpPlot(rf_fit,type=1)
	{/R}
{/left part of slide}
{right part of slide}
	# R Forest Fit using IncNodePurity
	{R}
		varImpPlot(rf_fit,type=2)
	{/R}
{/right part of slide}

====================================================================

Slide 4: Summary of R Forest 
* R2 above 0.6
* points by far most important in explainng price

====================================================================

Slide 4: R Forest Explainer

{left part of slide}
	# Interaction Title N Chars vs Points
	{R}
		plot_predict_interaction(rf_fit, wine_train, "title.n_chars", "points")
	{/R}
{/left part of slide}
{middle part of slide}
	# Interaction Title N Words vs Points
	{R}
		plot_predict_interaction(rf_fit, wine_train, "title.n_words", "points")
	{/R}
{/middle part of slide}
{right part of slide}
	# Interaction Taster Avg Pts vs Points
	{R}
		plot_predict_interaction(rf_fit, wine_train, "taster.avg_points", "points")
	{/R}
{/right part of slide}
