#decision tree

library(partykit)
library(party - plot)

decision_tree <- ctree(point_cat ~ color_lump,
                       data = wine_data_clean)


decision_tree2 <- ctree(point_cat ~ taster_gender,
                        data = wine_data_clean)

decision_tree3 <- ctree(point_cat ~ taster_name_lump,
                        data = wine_data_clean)

decision_tree4 <- ctree(point_cat ~ country_lump2,
                        data = wine_data_cleanc)

wine_data_cleanc <-
  wine_data_clean %>% dplyr::mutate(country_lump2 = fct_lump(country, 5))

glimpse(wine_data_temp_2)

decision_tree4 <- ctree(price_cat ~ ., data = wine_data_temp_3)

wine_data_temp_2 <-
  wine_data_temp %>% select_if(function(col)
    is.numeric(col) || is.factor(col))
wine_data_temp_3 <- wine_data_temp %>% select_if(is.factor)


plot(decision_tree)
plot(decision_tree2)
plot(decision_tree3)
plot(decision_tree4)
str(wine_data_clean)

wine_data_clean <- wine_data_clean %>% 
  dplyr::mutate(country = factor(country), 
         variety=factor(variety), 
         title_has_accents = as.numeric(title_has_accents)
         )

ggpairs(wine_data_clean, columns=c("price", "points", "title_length", "title_has_accents"),
diag=list(continuous="density",   discrete="bar"), axisLabels="show", ggplot2::aes(color=color_lump))

ggpairs(wine_data_clean, columns=c("price", "taster_avg_points", "taster_review_count", "taster_n_tweets" 
), diag=list(continuous="density",   discrete="bar"), axisLabels="show", ggplot2::aes(color=color_lump))

ggpairs(wine_data_clean, columns=c("price", "title_word_count", "title_sentement"), diag=list(continuous="density",   discrete="bar"), axisLabels="show", ggplot2::aes(color=color_lump))

ggpairs(wine_data_clean, diag=list(continuous="density",   discrete="bar"), axisLabels="show", ggplot2::aes(color=color_lump))
 


wine_data_cleanx <- wine_data_clean %>% select_if(is.numeric)
dput(names(wine_data_cleanx))

library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)

ggplot() + geom_bar(aes(y = , x = year, fill = product),
                    data = wine_data_clean,
                    stat = "identity")


library(rpart)
library(rpart.plot)

wine_data_temp <-
  wine_data_clean  %>%
  dplyr::mutate(price_cat = cut(
    price,
    breaks = c(0, 4, 12, 50, 200, 750, 3000),
    labels = c(
      "Budget ($0-4)",
      "Every Day ($5-12)",
      "Premium ($13-50)",
      "Ultra Premium ($51-200)",
      "Luxury ($201-750)",
      "What the fuck is wrong with you? ($751+)"
    )
  ))
dt <-
  rpart(
    price_cat ~ point_cat + country_lump + variety_lump + designation_lump + taster_name_lump,
    data = wine_data_temp,
    method = "class"
  )
dt <-
  rpart(price_cat ~ ., data = wine_data_temp_3, method = "class")


rpart.plot(dt)
rpart.plot(decision_tree)
rpart.plot(decision_tree2)
rpart.plot(decision_tree3)
rpart.plot(decision_tree4)

library(viridis)


qplot(
  data = wine_data_clean,
  x = color_lump,
  fill = price_cat,
  geom = "bar",
  #position = "fill"
) + labs(title = "Number of Wines",
         y = "Number of Wines",
         x = "Color",
         fill = "Price Category") + theme_clean()


ggplot(wine_data_clean %>% filter(price < 1000), aes(x=color_lump, y=price))

ggplot(wine_data_clean) + geom_bar(aes(x = color_lump, fill = price_cat)) + 
  labs(title = "Number of Wines", 
       y = "Number of Wines", 
       x = "Color", 
       fill = "Price Category") + 
  theme_clean()+
  coord_cartesian(ylim=c(0,30)) 
  ylim(0, 30)



ggplot(mpg) +
  geom_bar(aes(x = color_, fill = drv))
)
