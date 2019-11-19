library(partykit)
library(rpart)       
library(rpart.plot) 


#Mutate Data For Price Categories

wine_data_clean_tree <- wine_data_clean %>%  mutate (price_lump = cut(
  wine_data_clean$price,
  breaks = c(0, 4, 12, 50, 200, 750, 1000000),
  labels = c(
    "Budget ($(0-4)",
    "Every Day ($5-12)",
    "Premium ($13-50)",
    "Ultra Premium ($51-200)",
    "Luxury ($201-750)",
    "WTF is wrong with you? ($751+)"
  )
))

#Play Around With This Model

form <- as.formula(price_lump ~ country_lump + color_lump)
tree.1 <- rpart(form,data=wine_data_clean_tree,control=rpart.control(minsplit=20,cp=0))

prp(tree.1)

#Basic Price Model

decision_tree5 <- ctree(price_lump ~ point_cat, data = wine_data_clean_tree)



plot(decision_tree5, main = "Price Lump Vs Point_Cat")


#RPart Model (Price Lump and Point Cat)

wine_data_mod_tree <- rpart(price_lump ~ point_cat,
                             data = wine_data_clean_tree,
                             method = "class",
                             control = list(cp = 0, 
                                            minsplit = 10))
 
rpart.plot(wine_data_mod_tree, box.palette="RdBu", shadow.col="0", clip.right.labs = TRUE, varlen = 0, clip.facs = TRUE, extra = 101, under = TRUE, main = "Price Lump and Point Cat")
print(wine_data_mod_tree)
wine_data_mod_tree$cptable
summary(wine_data_mod_tree)

#Attempt at One with Country_Lump

wine_data_mod_tree2 <- rpart(price_lump ~ country_lump,
                            data = wine_data_clean_tree,
                            method = "anova",
                            control = list(cp = 0.001, 
                                           minsplit = 10))

rpart.plot(wine_data_mod_tree2, box.palette="RdBu", shadow.col="0", clip.right.labs = TRUE, varlen = 0, clip.facs = TRUE, extra = 100, under = TRUE, main = "Price Lump and Country Lump")

#Random Simple Trees

wine_data_cleanc <- wine_data_clean %>% mutate(country_lump2 = fct_lump(country, 5))

decision_tree <- ctree(point_cat ~ color_lump, 
                       data = wine_data_clean)


decision_tree2 <- ctree(point_cat ~ taster_gender, 
                        data = wine_data_clean)

decision_tree3 <- ctree(point_cat ~ taster_name_lump,
                        data = wine_data_clean)

decision_tree4 <- ctree(point_cat ~ country_lump2,
                        data = wine_data_cleanc)

plot(decision_tree)
prp(decision_tree)
plot(decision_tree2)
plot(decision_tree3)
plot(decision_tree4)
