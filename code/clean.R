################### Clean ###################

class(wine_data)
# [1] "data.frame"


dim(wine_data)
# [1] 129971     14

names(wine_data)
# [1] "X"                     "country"               "description"           "designation"           "points"                "price"                
# [7] "province"              "region_1"              "region_2"              "taster_name"           "taster_twitter_handle" "title"                
# [13] "variety"               "winery"      

# TODO Confirm relevant columns currently I think country, designation, points, price, taster_name, title, variety and maybe winery

str(wine_data)
# 'data.frame':	129971 obs. of  14 variables:
# $ X                    : int  0 1 2 3 4 5 6 7 8 9 ...
# $ country              : Factor w/ 44 levels "","Argentina",..: 24 33 44 44 44 39 24 17 19 17 ...
# $ description          : Factor w/ 119955 levels ". A delightfully intriguing “White Burgundy” blend of Chardonnay, Pinot Blanc and Pinot Meunier, the last an un"| __truncated__,..: 16428 99129 75515 60554 55751 22175 43238 89260 67624 91167 ...
# $ designation          : Factor w/ 37980 levels "","??? Vineyard",..: 36977 2353 1 28124 36716 1997 3052 1 30972 20047 ...
# $ points               : int  87 87 87 87 87 87 87 87 87 87 ...
# $ price                : num  NA 15 14 13 65 15 16 24 12 27 ...
# $ province             : Factor w/ 426 levels "","Achaia","Aconcagua Costa",..: 335 111 270 221 270 264 335 13 311 13 ...
# $ region_1             : Factor w/ 1230 levels "","Abruzzo","Adelaida District",..: 426 1 1219 551 1219 759 1206 23 1 23 ...
# $ region_2             : Factor w/ 18 levels "","California Other",..: 1 1 18 1 18 1 1 1 1 1 ...
# $ taster_name          : Factor w/ 20 levels "","Alexander Peartree",..: 11 17 16 2 16 14 11 17 3 17 ...
# $ taster_twitter_handle: Factor w/ 16 levels "","@AnneInVino",..: 6 12 9 1 9 14 6 12 1 12 ...
# $ title                : Factor w/ 118840 levels ":Nota Bene 2005 Una Notte Red (Washington)",..: 79669 89457 89940 101059 102995 103740 105794 108715 54638 59312 ...
# $ variety              : Factor w/ 708 levels "","Abouriou",..: 693 453 439 482 443 594 189 212 212 439 ...
# $ winery               : Factor w/ 16757 levels ":Nota Bene","1+1=3",..: 11641 12988 13054 14432 14665 14740 15046 15435 8433 9014 ...


glimpse(wine_data)
# Observations: 129,971
# Variables: 14
# $ X                     <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, …
# $ country               <fct> Italy, Portugal, US, US, US, Spain, Italy, France, Germany, France, US, France, US, Italy, US, Germany, Argentina, Argentina, Spain, US, U…
# $ description           <fct> "Aromas include tropical fruit, broom, brimstone and dried herb. The palate isn't overly expressive, offering unripened apple, citrus and …
# $ designation           <fct> Vulkà Bianco, Avidagos, , Reserve Late Harvest, Vintner's Reserve Wild Child Block, Ars In Vitro, Belsito, , Shine, Les Natures, Mountain …
# $ points                <int> 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 86, 86, 86, 86, 86, 86…
# $ price                 <dbl> NA, 15, 14, 13, 65, 15, 16, 24, 12, 27, 19, 30, 34, NA, 12, 24, 30, 13, 28, 32, 23, 20, 19, 22, 35, 69, 13, 10, 17, 16, NA, NA, NA, 50, 20…
# $ province              <fct> Sicily & Sardinia, Douro, Oregon, Michigan, Oregon, Northern Spain, Sicily & Sardinia, Alsace, Rheinhessen, Alsace, California, Alsace, Ca…
# $ region_1              <fct> Etna, , Willamette Valley, Lake Michigan Shore, Willamette Valley, Navarra, Vittoria, Alsace, , Alsace, Napa Valley, Alsace, Alexander Val…
# $ region_2              <fct> , , Willamette Valley, , Willamette Valley, , , , , , Napa, , Sonoma, , Central Coast, , , , , , , Oregon Other, , Central Coast, , Sonoma…
# $ taster_name           <fct> Kerin O’Keefe, Roger Voss, Paul Gregutt, Alexander Peartree, Paul Gregutt, Michael Schachner, Kerin O’Keefe, Roger Voss, Anna Lee C. Iijim…
# $ taster_twitter_handle <fct> @kerinokeefe, @vossroger, @paulgwine , , @paulgwine , @wineschach, @kerinokeefe, @vossroger, , @vossroger, @vboone, @vossroger, @vboone, @…
# $ title                 <fct> Nicosia 2013 Vulkà Bianco  (Etna), Quinta dos Avidagos 2011 Avidagos Red (Douro), Rainstorm 2013 Pinot Gris (Willamette Valley), St. Julia…
# $ variety               <fct> White Blend, Portuguese Red, Pinot Gris, Riesling, Pinot Noir, Tempranillo-Merlot, Frappato, Gewürztraminer, Gewürztraminer, Pinot Gris, C…
# $ winery                <fct> Nicosia, Quinta dos Avidagos, Rainstorm, St. Julian, Sweet Cheeks, Tandem, Terre di Giurfo, Trimbach, Heinz Eifel, Jean-Baptiste Adam, Kir…


dput(colnames(wine_data))
# c("X", "country", "description", "designation", "points", "price", 
#   "province", "region_1", "region_2", "taster_name", "taster_twitter_handle", 
#   "title", "variety", "winery")


# Drop unimportant columns for summary
sum_temp <- wine_data %>% select(c(country, designation,points, price, taster_name, title, variety, winery
))
# country            designation        points           price                    taster_name                                                        title       
# US      :54504               :37465   Min.   : 80.00   Min.   :   4.00                    :26244   Gloria Ferrer NV Sonoma Brut Sparkling (Sonoma County) :    11  
# France  :22093   Reserve     : 2009   1st Qu.: 86.00   1st Qu.:  17.00   Roger Voss       :25514   Korbel NV Brut Sparkling (California)                  :     9  
# Italy   :19540   Estate      : 1322   Median : 88.00   Median :  25.00   Michael Schachner:15134   Segura Viudas NV Extra Dry Sparkling (Cava)            :     8  
# Spain   : 6645   Reserva     : 1259   Mean   : 88.45   Mean   :  35.36   Kerin O’Keefe    :10776   Gloria Ferrer NV Blanc de Noirs Sparkling (Carneros)   :     7  
# Portugal: 5691   Riserva     :  698   3rd Qu.: 91.00   3rd Qu.:  42.00   Virginie Boone   : 9537   Ruinart NV Brut Rosé  (Champagne)                      :     7  
# Chile   : 4472   Estate Grown:  621   Max.   :100.00   Max.   :3300.00   Paul Gregutt     : 9532   Segura Viudas NV Aria Estate Extra Dry Sparkling (Cava):     7  
# (Other) :17026   (Other)     :86597                    NA's   :8996      (Other)          :33234   (Other)                                                :129922  
#                      variety                     winery      
#  Pinot Noir              :13272   Wines & Winemakers:   222  
#  Chardonnay              :11753   Testarossa        :   218  
#  Cabernet Sauvignon      : 9472   DFJ Vinhos        :   215  
#  Red Blend               : 8946   Williams Selyem   :   211  
#  Bordeaux-style Red Blend: 6915   Louis Latour      :   199  
#  Riesling                : 5189   Georges Duboeuf   :   196  
#  (Other)                 :74424   (Other)           :128710  

# TODO Clean up columns
# Country - Convert to character for joining and remove 
#     

summary(sum_temp)
  
  hist(wine_data$price)
  plot(wine_data$price, wine_data$points)
  
  wine_data <-
    wine_data %>% mutate (country = as.character(country), variety = as.character(variety)) %>%
    filter (
      country != "England" &
        country != "US-France" &
        country != "" &
        variety != ""
    ) %>% drop_na(price)
  
  # TODO
  