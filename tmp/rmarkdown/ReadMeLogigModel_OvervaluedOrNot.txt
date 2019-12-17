as reference if need be use: code/temp/markdown/logitmodel_OvervaluedOrNot.RMD

====================================================================
Slide 1: Determining whether wine good value or not

Logit Model
	logit_mod <- glm( well_priced ~ ...... )

How is Well_Priced determined?
	Well_priced == whether we think wine is well priced
	Well_priced is determined according to a median price to points ratio computed as follows using observable from the training data set
		Dataset ratio numerator = Median (points awarded) - Min (points awarded)
		Dataset ratio denominator = Median ( log ( price of wine)) - Min (log (expected(min price of drinkable wine))

In Summary: Well_Priced takes into consideration diminishing returns i.e. marginal increase in points is accompanied by a higher and higher increase in price 

=============================

Slide 2: Points vs Price by Well Pricsed 

Purpose: showing the effects of dimishing returns

{r}
ggplot(wine_train_logit , aes(x = price, y = points, color = well_priced)) +
  geom_jitter() +
  theme(legend.position = "top") + 
  labs(title="Price and Points Colored by Well Priced", 
       color = "Well Priced") 
{/r}


=============================

Slide 3: Model Summary

Notes: Price removed from the dataset + taster.twitter_handle and variety_and_color also removed as they provide no added value (i.e. NA values)

<Bart Note: not too sure how to fit this>

<R split in 3 columns ??? >
		summary(logit_mod)
</R>

=============================

Slide 4: ROC Curves and AUC -- Train vs Test

<left part of Slide>
	<R>
		plot(TrainROC)
	</R>
	<R> 
		calc_auc(TrainROC)
	</R>
</left part of Slide>

<right part of Slide>
	<R>
		plot(TestROC)
	</R>
	<R> 
		calc_auc(TestROC)
	</R>
</right part of Slide>

=============================

Slide 5: Summary of Model 
* train and test set curves above diagonal chance-only line
	* determining whether wine is a “good value” better than chance
* AUC values are high, above 80% or about 70% higher than chance
* AUC for train and test nearly identical (0.868 vs 0.867)== model neither over- or underfit
* Points, Specific Province, Specific Variety, Specific Winery, followed by Specific Taster explain the model best whereas Title information not so much
	