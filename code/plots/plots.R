###############################################################################-
#                                                                              #
# Purpose:       A general repository for plots                                #
#                                                                              #
# Author:        Garrett H.                                                    #
# Contact:       hawes102@mail.chapman.edu                                     #
# Client:        Garrett H.                                                    #
#                                                                              #
# Code created:  2019-11-23                                                    #
# Last updated:  2019-11-23                                                    #
# Source:        /Users/garretthawes/wine-project                              #
#                                                                              #
# Comment:       Check side menu to navigate                                   #
#                                                                              #
###############################################################################-

################################ CHOROPLETH MAP ################################

world_map <- map_data("world")
world_map <-
  world_map %>% dplyr::mutate(region = ifelse(region == "USA", "US", region))
world_map <- world_map %>% dplyr::mutate(country = region)

wine_map_DF <-
  wine_data_clean  %>% dplyr::mutate(country = as.character(country)) %>%
  dplyr::filter (country != "England" &
                   country != "US-France" &
                   country != "")

# Generate Summary
wmap <-
  wine_data_clean %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(
    point_min = min(points, na.rm = TRUE),
    point_avg = mean(points, na.rm = TRUE),
    point_max = max(points, na.rm = TRUE),
    price_min = min(price, na.rm = TRUE),
    price_avg = mean(price, na.rm = TRUE),
    price_max = max(price, na.rm = TRUE),
    count = n()
  ) %>%
  dplyr::select(
    c(
      "country",
      "point_min",
      "point_avg",
      "point_max",
      "price_min",
      "price_avg",
      "price_max",
      "count"
    )
  )
# Create data frame with bounderies and values
wine_country_map <- right_join(wmap, world_map, by = "country")

#scale_fill_viridis_c(option = "C", limits = c(80, 100))

.map_from_attribute <- function(att, title) {
  return(
    ggplot(wine_country_map, aes_string(map_id = "country", fill = att)) +
      geom_map(map = wine_country_map,  color = "white") +
      expand_limits(x = wine_country_map$long, y = wine_country_map$lat) +
      scale_fill_viridis_c(option = "C") + theme_map() + ggtitle(title) + 
      theme(plot.title = element_text(hjust = 0.5, size=20)) +
      theme(legend.title = element_blank()) 
  )
}

.map_from_attribute("point_min", "Minimum Points")
.map_from_attribute("point_avg", "Average Points")
.map_from_attribute("point_max", "Maximum Points") 
.map_from_attribute("point_min", "Minimum Price")
.map_from_attribute("price_avg", "Average Price") 
.map_from_attribute("price_max", "Maximum Price") 


################################## Corelation ##################################


ggpairs(wine_data_clean, columns = c(2,5,7, 14, 16), 
        ggplot2::aes(color=color_lump))
dput(wine_data_clean)


################################ Price v Points ################################


p <- ggplot(wine_data_clean, aes(points, price, color = point_cat)) + 
  geom_point() + 
  theme_clean() + 
  labs(color = "Rating Category", title = "Price vs Points") +
  theme(plot.title = element_text(hjust = 0.5)) 
p1 <- ggMarginal(p, type="histogram", fill="slateblue") 
p1 

####################### Price v Points (y log 10 scale) ########################

p <- ggplot(wine_data_clean, 
            aes(points, price, color = point_cat)) + 
  geom_point() + 
  theme_clean() + 
  scale_y_log10() + 
  labs(color = "Rating Category", 
       title = "Price vs Score", 
       caption="Price is scaled to log 10", 
       x = "Points", 
       y="Price") +
  theme(plot.title = element_text(hjust = 0.5)) 
p1 <- ggMarginal(p, type="histogram", fill="slateblue") 
p1 

################################### Box Plot ###################################

wine_data_clean %>% 
  ggplot( aes(x = reorder(color_lump, points), 
              y = points, 
              fill = color_lump)) + 
  geom_boxplot() + 
  xlab("Color") +
  labs(x="Color", 
       y="Points", 
       title="Wine Score by color", 
       caption="Faceted by Gender") + 
  theme_clean() + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~ country_lump)


commentr::line_comment("Geom Hex")

################################### Geom Hex ###################################

ggplot(data = wine_data_clean) +
  geom_hex(mapping = aes(x = points, y = price)) +
  scale_y_log10()

####################### Price v Score faceted by Gender ########################

ggplot(wine_data_clean, 
       aes(points, price, color = point_cat)) + 
  geom_point() + 
  scale_y_log10()+
  theme_clean() + 
  labs(color = "Rating Category", 
       title = "Price v Score by Taster Gender", 
       x="Score", 
       y="Price", 
       caption="Price is scaled to log 10" ) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~ taster_gender)

########################## Price v Points by Country ###########################

ggplot(wine_data_clean, 
       aes(y=price, 
           x=points, 
           color = point_cat)) + 
  geom_point(alpha=1/2) + 
  geom_smooth() +
  scale_y_log10()+
  theme_clean()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(color="Rating Category", 
       title = "Score vs log of Price") + 
  facet_wrap(~ country_lump)

############################### Price by Country ###############################

ggplot(wine_data_clean, 
       aes(y = price, 
           x = country_lump, 
           color = point_cat)) +
  theme_clean()+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  geom_jitter(alpha=1/2) + 
  labs(color="Rating Category", 
       title="Price and Country Colored by Rating Category", 
       x="Country", 
       y="Price")

######################## Country v Price (log10 scale) #########################

ggplot(wine_data_clean, 
       aes(x = country_lump, 
           y = price, 
           color = point_cat)) + 
  scale_y_log10() +
  geom_jitter(alpha=1/2) + 
  coord_flip() +
  labs(color = "Rating Category", 
       title="Price by Country", 
       caption="Price is scaled to log 10", 
       x = "Country", 
       y="Price") +
  theme_clean()  +
  theme(plot.title = element_text(hjust = 0.5))

########################## Taster Tweets v log(Price) ##########################

ggplot(wine_data_clean, 
       aes(y = price, 
           x = taster_n_tweets, 
           color = point_cat)) +
  theme_clean()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_log10()+
  scale_y_log10()+
  geom_jitter(alpha=1/2) + 
  labs(color="Rating Category", 
       title="Price v Number of Tweets", 
       x="Number of Tweets from Taster", 
       y="Price", 
       caption="Price and Teets Scaled to log 10")



