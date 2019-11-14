################### CHOROPLETH MAP ###################

#TODO Look at this for faceted maps, also look at plotly

# Get Lon and Lat to join then replace USA with US since our data set lists it as US also add country column for join

world_map <- map_data("world")
world_map <-
  world_map %>% 
  dplyr::mutate(region = ifelse(region == "USA", "US", region)) 
colnames(world_map)[colnames(world_map)=="region"] <- "country"
head(world_map)



wmap <-
  wine_data_clean %>%
  dplyr::filter(country!="England")%>%
  dplyr::group_by(country) %>%
  dplyr::summarize(
    n = n(),
    point_min = min(points, na.rm = TRUE),
    point_avg = mean(points, na.rm = TRUE),
    point_max = max(points, na.rm = TRUE),
    price_min = min(price, na.rm = TRUE),
    price_avg = mean(price, na.rm = TRUE),
    price_max = max(price, na.rm = TRUE)
  ) %>% dplyr::ungroup()

wine_country_map <- merge(wmap, world_map, by = "country")


.map_from_attribute <- function(att, title) {
  return(
    ggplot(wine_country_map, aes_string(map_id = "country", fill = att)) +
      geom_map(map = wine_country_map,  color = "white") +
      expand_limits(x = wine_country_map$long, y = wine_country_map$lat) +
      scale_fill_viridis_c(option = "C") + theme_gdocs() + ggtitle(title)
  )
}

?geom_map

glimpse(wine_data_clean)

.map_from_attribute("point_min", "Points Min by Country")
# .save_pdf("point_min")
.map_from_attribute("point_avg", "Points Average by Country")
# .save_pdf("point_avg")
.map_from_attribute("point_max", "Points Max by Country")
# .save_pdf("point_max")
.map_from_attribute("price_min", "Price Min by Country")
# .save_pdf("price_min")
.map_from_attribute("price_avg", "Price Average by Country")
# .save_pdf("price_avg")
.map_from_attribute("price_max", "Price Max by Country")
# .save_pdf("price_max")


# c(1,2,5,7,14,15)
#chart.Correlation(wine_data_clean[,c(2,5,7,14,15)],col=wine_data_clean$color_lump)



# FANCY CORELATION MATRIX -------------------------------------------------

# Fix conflicts in ggpairs
conflict_prefer("mutate", "dplyr")
conflict_prefer("summarize", "dplyr")

ggpairs(wine_data_clean, columns = c(2,5,7,14, 16), ggplot2::aes(colour=color_lump)) 

glimpse(wine_data_clean)

# %>%
#   select(
#     c(
#       "country",
#       "point_min",
#       "point_avg",
#       "point_max",
#       "price_min",
#       "price_avg",
#       "price_max"
#     )
#   ) %>% ungroup() 

#wine_data %>% filter( points == NA )


ggplot(wine_data_clean, aes(log(price), points, color = point_cat)) + geom_point() + theme_fivethirtyeight() + labs(title = "Score vs Price") 
  
ggplot(wine_data_clean, aes(price, points, color = point_cat)) + geom_point() + theme_fivethirtyeight() + labs(title = "Score vs Price") #+ xlim(0, 100)


ggplot(wine_data_clean, aes(price, points, color = point_cat)) + geom_jitter(alpha =
                                                                         1 / 10) + theme_fivethirtyeight() + labs(title = "Score vs Price") + xlim(0, 100)


ggplot(wine_data_clean, aes(price, points, color = point_cat)) + geom_point() + theme_fivethirtyeight() + labs(title = "Score vs Price") + xlab("Points (0-100)") +
  ylab("Price ($)")


# TODO put this somewhere
color_sum <-
  wine_data_with_color %>%
  group_by(color) %>%
  summarize( count = n(),
    point_min = min(points, na.rm = TRUE),
    point_avg = round(mean(points, na.rm = TRUE),2),
    point_max = max(points, na.rm = TRUE),
    price_min = min(price, na.rm = TRUE),
    price_avg = round(mean(price, na.rm = TRUE),2),
    price_max = max(price, na.rm = TRUE)) 


color_sum


wine_data %>% ggplot(aes(x = reorder(color, points), y = points, fill = color)) + geom_boxplot() + 
  xlab("Color") + theme_clean() #+ theme(legend.position = "none") 

ggplot(wine_data, aes(points, price, colour = color)) + geom_point() + theme_gdocs()

ggplot(wine_data, aes(points, price, color = point_cat)) + geom_point() + theme_fivethirtyeight() + labs(title = "Score vs Price")


p <- ggplot(wine_data, aes(points, price, color = point_cat)) + geom_point() + theme_gdocs() + labs(title = "Price vs Points")  #+ facet_wrap(~ color_lump + taster_gender)
p1 <- ggMarginal(p, type="histogram", fill="slateblue") 
p1  

ggplot(wine_data_clean, aes(y=points, x=log(price), colour = color_lump)) + geom_point() + theme_fivethirtyeight() + facet_grid( taster_gender ~ .)
  


wine_data_clean %>% ggplot(aes(x = reorder(color, points), y = points, fill = color)) + geom_boxplot() + 
  xlab("Color") + theme_clean() + facet_wrap(~ taster_gender) #+ theme(legend.position = "none") 

wine_data_clean %>% ggplot(aes(x = reorder(color_lump, points), y = points, fill = color_lump)) + geom_boxplot() + 
  xlab("Color") + theme_clean() + facet_wrap(~ taster_gender + title_has_accents) + theme(legend.position = "none") + ggtitle("Wine Score vs color. Faceted by Gender and presence of accents in title")

str(wine_data_clean)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	91500 obs. of  19 variables:
# $ ID                 : int  1 2 3 4 5 6 7 8 9 10 ...
# $ price              : num  15 65 15 16 19 34 30 13 28 20 ...
# $ country            : chr  "Portugal" "US" "Spain" "Italy" ...
# $ variety            : chr  "Portuguese Red" "Pinot Noir" "Tempranillo-Merlot" "Frappato" ...
# $ points             : int  87 87 87 87 87 87 87 87 87 87 ...
# $ point_cat          : Factor w/ 4 levels "Good","Very good",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ title_length       : int  45 83 53 48 71 59 42 55 98 32 ...
# $ title_has_accents  : logi  FALSE FALSE FALSE FALSE TRUE FALSE ...
# $ variety_lump       : Factor w/ 11 levels "Bordeaux-style Red Blend",..: 11 5 11 11 2 2 4 4 11 5 ...
# $ designation_lump   : Factor w/ 11 levels "Barrel","Brut",..: 11 8 11 11 1 5 11 9 11 5 ...
# $ taster_name_lump   : Factor w/ 6 levels "Kerin O’Keefe",..: 4 3 2 1 5 5 2 2 2 3 ...
# $ taster_twitter_lump: Factor w/ 6 levels "@kerinokeefe",..: 4 2 5 1 3 3 5 5 5 2 ...
# $ taster_gender      : Factor w/ 2 levels "F","M": 2 2 2 1 1 1 2 2 2 2 ...
# $ taster_avg_points  : num  88.6 89.1 86.9 88.9 89.2 ...
# $ taster_review_count: int  20172 9497 14944 9874 9507 9507 14944 14944 14944 9497 ...
# $ taster_n_tweets    : Factor w/ 15 levels "1,002","1,042",..: 15 7 3 12 5 5 3 3 3 7 ...
# $ color_lump         : Factor w/ 3 levels "Red","White",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ country_lump       : Factor w/ 11 levels "Argentina","Australia",..: 8 10 9 6 10 10 1 1 9 10 ...
# $ province_lump      : Factor w/ 11 levels "Bordeaux","Burgundy",..: 11 6 5 11 3 3 11 4 5 6 ...

summary(wine_data_clean)
# ID            price           country            variety              points             point_cat      title_length    title_has_accents
# Min.   :    1   Min.   :   4.00   Length:91500       Length:91500       Min.   : 80.00   Good       : 7465   Min.   : 12.00   Mode :logical    
# 1st Qu.:22876   1st Qu.:  17.00   Class :character   Class :character   1st Qu.: 87.00   Very good  :47803   1st Qu.: 43.00   FALSE:65322      
# Median :45750   Median :  25.00   Mode  :character   Mode  :character   Median : 88.00   Outstanding:34576   Median : 52.00   TRUE :26178      
# Mean   :45750   Mean   :  35.29                                         Mean   : 88.61   Classic    : 1656   Mean   : 52.75                    
# 3rd Qu.:68625   3rd Qu.:  42.00                                         3rd Qu.: 91.00                       3rd Qu.: 61.00                    
# Max.   :91500   Max.   :3300.00                                         Max.   :100.00                       Max.   :136.00                    
# 
# variety_lump         designation_lump          taster_name_lump   taster_twitter_lump taster_gender taster_avg_points taster_review_count
# Other                   :41626   Other         :34918   Kerin O’Keefe    : 9874   @kerinokeefe: 9874    F:25583       Min.   :86.61     Min.   :    6      
# Pinot Noir              : 9622   No Designation:25584   Michael Schachner:14944   @paulgwine  : 9497    M:65917       1st Qu.:88.54     1st Qu.: 6237      
# Chardonnay              : 7935   Reserve       : 8815   Paul Gregutt     : 9497   @vboone     : 9507                  Median :88.63     Median : 9507      
# Red Blend               : 6737   Some Vineyard : 8199   Roger Voss       :20172   @vossroger  :20172                  Mean   :88.61     Mean   :11257      
# Cabernet Sauvignon      : 6090   Estate        : 4790   Virginie Boone   : 9507   @wineschach :14944                  3rd Qu.:89.09     3rd Qu.:14944      
# Bordeaux-style Red Blend: 4700   Brut          : 2390   Other            :27506   Other       :27506                  Max.   :90.61     Max.   :20172      
# (Other)                 :14790   (Other)       : 6804                                                                                                      
# taster_n_tweets color_lump      country_lump          province_lump  
# 803    :20172   Red  :57244   US      :34535   Other         :37065  
# 1,212  :14944   White:28103   France  :17525   California    :20042  
# 3,007  : 9874   Other: 6153   Italy   :10121   Washington    : 8575  
# 1,284  : 9507                 Spain   : 6509   Oregon        : 5323  
# 1,791  : 9497                 Portugal: 4870   Bordeaux      : 3990  
# 1,534  : 6237                 Chile   : 4305   Northern Spain: 3769  
# (Other):21269                 (Other) :13635   (Other)       :12736  
  