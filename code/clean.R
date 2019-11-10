################### Clean ###################

wine_data_original <- wine_data

# rollback wine data if there is a change that you don't like
.rollback_wine_date <- function() {
  wine_data <- wine_data_orignal
}

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

wine_tibble <- as_tibble(wine_data)

str(wine_tibble)
wine_tibble

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

sum_temp <-
  wine_data %>% select(c(
    country,
    designation,
    points,
    price,
    taster_name,
    title,
    variety,
    winery,
    color
  ))

summary(sum_temp)

# country            designation        points           price                    taster_name
# US      :54504               :37464   Min.   : 80.00   Min.   :   4.00                    :26243
# France  :22093   Reserve     : 2009   1st Qu.: 86.00   1st Qu.:  17.00   Roger Voss       :25514
# Italy   :19540   Estate      : 1322   Median : 88.00   Median :  25.00   Michael Schachner:15134
# Spain   : 6645   Reserva     : 1259   Mean   : 88.45   Mean   :  35.36   Kerin O’Keefe    :10776
# Portugal: 5691   Riserva     :  698   3rd Qu.: 91.00   3rd Qu.:  42.00   Virginie Boone   : 9537
# Chile   : 4471   Estate Grown:  621   Max.   :100.00   Max.   :3300.00   Paul Gregutt     : 9532
# (Other) :17026   (Other)     :86597                    NA's   :8996      (Other)          :33234
#                                                      title                            variety                     winery       color
#  Gloria Ferrer NV Sonoma Brut Sparkling (Sonoma County) :    11   Pinot Noir              :13272   Wines & Winemakers:   222   O : 4949
#  Korbel NV Brut Sparkling (California)                  :     9   Chardonnay              :11753   Testarossa        :   218   R :79658
#  Segura Viudas NV Extra Dry Sparkling (Cava)            :     8   Cabernet Sauvignon      : 9472   DFJ Vinhos        :   215   SW: 3632
#  Gloria Ferrer NV Blanc de Noirs Sparkling (Carneros)   :     7   Red Blend               : 8946   Williams Selyem   :   211   W :41731
#  Ruinart NV Brut Rosé  (Champagne)                      :     7   Bordeaux-style Red Blend: 6915   Louis Latour      :   199
#  Segura Viudas NV Aria Estate Extra Dry Sparkling (Cava):     7   Riesling                : 5189   Georges Duboeuf   :   196
#  (Other)                                                :129921   (Other)                 :74423   (Other)           :128709

# TODO Clean up columns [x] means done [ ] means still left, also there could be more that is just what i observed

#   country         [x] Convert to character for joining and remove missing countries
#   designation     [x] Clean
#                   [ ] Look at the various things like Reserva, Reserve, Riserve etc... as a feature or normalize here
#   points          [x] looks good
#                   [ ] maybe add scaled column [0-20]
#   price           [x] drop na's (8,996 obs)
#                   [ ] maybe filter outliers (talk to hersh)
#   taster_name     [x] filter out wines that are missing a taster (26,244 obs)
#   title           [x] seems good but this should not be a factor since they are all distinct convert to character
#                     and drop after feature engeneering
#   variety         [ ] Seems clean, factor_lump maybe although group by white and red somehow? Ask me what this means but i
#                     have a specific goal.
#   winery          [ ] Drop this column they are mostly unique, maybe do something with sentement analysis on name
#   color           [x] fct lump as R, W and Other, then rename to "Red", "White", "Other" - NOTE: Does SW go with other or with white?



# These are just quick dirty plots so i can get a sense of the data

boxplot(wine_data$price)
hist(wine_data$price)
plot(wine_data$price, wine_data$points)


# Rename from R W O SW

wine_data <-
  wine_data %>% mutate (color = revalue(
    wine_data$color,
    c(
      "O" = "Other",
      "R" = "Red",
      "W" = "White",
      "SW" = "Sparkling White"
    )
  ))

# Apply the cleaning strategies from above

wine_data <-
  wine_data %>% mutate (
    country = as.character(country),
    variety = as.character(variety),
    taster_name = as.character(taster_name),
    title = as.character(title),
    color_simple = fct_lump(color, n = 2)
  ) %>%
  filter (country != "" &
            variety != "") %>% drop_na(price)

wine_data$color_simple


#


# Deal with reserva all of this is just playing around with getting reserve dealt with

# inspections <- wine_data
#
# wine_data%>%
#   group_by(designation) %>%
#   summarize(designations=n()) %>%
#   arrange(desc(designations)) %>% View()


# Find alternate spellings of reserva
# inspections %>%
#   filter(grepl("serv", designation, ignore.case=TRUE)) %>%
#   select(designation) %>%
#   unique() %>% View()
#
#
# alternates <- inspections %>%
#     filter(grepl("serv", designation, ignore.case=TRUE)) %>%
#     select(designation) %>%
#     unique() %>%
#     pull(designation)
#
#
# inspections <- inspections %>% mutate(designation=ifelse(designation %in% alternates, 'Reserva', designation))
#
# view(inspections)

# Check most inspected restaurants again

# inspections %>%
#   group_by(RestaurantName) %>%
#   summarize(inspections=n()) %>%
#   arrange(desc(inspections))


# %>%
#   filter(RestaurantName!='SARAH MCDONALD STEELE') %>%
#   select(RestaurantName) %>%
#   unique() %>%
#   View()

# Create a vector of those alternate spellings

# alternates <- wine_data %>%
#   filter(grepl("McDo", RestaurantName, ignore.case=TRUE)) %>%
#   filter(RestaurantName!='SARAH MCDONALD STEELE') %>%
#   select(RestaurantName) %>%
#   unique() %>%
#   pull(RestaurantName)

# Replace them all with MCDONALDS

# inspections <- inspections %>%
#   mutate(RestaurantName=ifelse(RestaurantName %in% alternates, 'MCDONALDS', RestaurantName))

# Check most inspected restaurants again

# inspections %>%
#   group_by(RestaurantName) %>%
#   summarize(inspections=n()) %>%
#   arrange(desc(inspections))
