################### LOAD DATA ###################
wine_data <-
  read.csv(here("data", "input", "winemag-data-130k-v2.csv"))
colors <- read.csv(here("data", "input", "wine-colors.csv"))

################### Clean ###################
# TODO Look up how to drop blank factor for country 

################### JOIN ###################
wine_data_with_color <- merge(wine_data, colors, by = "variety")

# 3428 records not matching
nrow(wine_data)
nrow(wine_data_with_color)



################### CHOROPLETH MAP ###################

#TODO Look at this for faceted maps, also look at plotly

# Get Lon and Lat to join then replace USA with US since our data set lists it as US also add country column for join

world_map <- map_data("world")
world_map <-
  world_map %>% mutate(region = ifelse(region == "USA", "US", region))
world_map <- world_map %>% mutate (country = region)

# Drop England, US-France and blank for simplicity since there is no entry in world_map

wine_map_DF <-
  wine_data  %>% mutate (country = as.character(country)) %>%
  filter (country != "England" &
            country != "US-France" &
            country != "")
head(wine_map_DF)
# Generate Summary

wmap <-
  wine_data %>%
  group_by(country) %>%
  summarize(
    point_min = min(points, na.rm = TRUE),
    point_avg = mean(points, na.rm = TRUE),
    point_max = max(points, na.rm = TRUE),
    price_min = min(price, na.rm = TRUE),
    price_avg = mean(price, na.rm = TRUE),
    price_max = max(price, na.rm = TRUE)
  ) %>%
  select(
    c(
      "country",
      "point_min",
      "point_avg",
      "point_max",
      "price_min",
      "price_avg",
      "price_max"
    )
  ) 

head(wmap)

# Create data frame with bounderies and values
wine_country_map <- left_join(wmap, world_map, by = "country")

.to_PNG <- function(pl, filename){
  png('images/rplot.png')
  plot(pl)
  dev.off()
}

.map_from_attribute <- function(att, title) {
  return(ggplot(wine_country_map, aes_string(map_id = "country", fill = att)) +
    geom_map(map = wine_country_map,  color = "white") +
    expand_limits(x = wine_country_map$long, y = wine_country_map$lat) +
    scale_fill_viridis_c(option = "C") + theme_fivethirtyeight() + ggtitle(title))
}

.map_from_attribute("point_min", "Points Min by Country")
.map_from_attribute("point_avg", "Points Average by Country")
.map_from_attribute("point_max", "Points Max by Country")
.map_from_attribute("price_min", "Price Min by Country")
.map_from_attribute("price_avg", "Price Average by Country")
.map_from_attribute("price_max", "Price Max by Country")
