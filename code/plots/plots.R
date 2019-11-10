################### CHOROPLETH MAP ###################

#TODO Look at this for faceted maps, also look at plotly

# Get Lon and Lat to join then replace USA with US since our data set lists it as US also add country column for join

world_map <- map_data("world")
world_map <-
  world_map %>% mutate(region = ifelse(region == "USA", "US", region)) %>% mutate (country = region) %>% mutate(country = ifelse(subregion == "Great Britain", "England", country))



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

.save_pdf <- function(fn) {
  ggsave(paste0("images/", fn, ".pdf"),
         width = 6,
         height = 5)
}

.map_from_attribute <- function(att, title) {
  return(
    ggplot(wine_country_map, aes_string(map_id = "country", fill = att)) +
      geom_map(map = wine_country_map,  color = "white") +
      expand_limits(x = wine_country_map$long, y = wine_country_map$lat) +
      scale_fill_viridis_c(option = "C") + theme_map() + ggtitle(title)
  )
}

.map_from_attribute("point_min", "Points Min by Country")
.save_pdf("point_min")
.map_from_attribute("point_avg", "Points Average by Country")
.save_pdf("point_avg")
.map_from_attribute("point_max", "Points Max by Country")
.save_pdf("point_max")
.map_from_attribute("price_min", "Price Min by Country")
.save_pdf("price_min")
.map_from_attribute("price_avg", "Price Average by Country")
.save_pdf("price_avg")
.map_from_attribute("price_max", "Price Max by Country")
.save_pdf("price_max")




ggplot(wine_data, aes(price, points, color = point_cat)) + geom_point() + theme_fivethirtyeight() + labs(title = "Score vs Price") + xlim(0, 1000) +
  
  ggplot(wine_data, aes(price, points, color = point_cat)) + geom_point() + theme_fivethirtyeight() + labs(title = "Score vs Price") + xlim(0, 100)

ggplot(wine_data, aes(price, points, color = point_cat)) + geom_jitter(alpha =
                                                                         1 / 10) + theme_fivethirtyeight() + labs(title = "Score vs Price") + xlim(0, 100)


ggplot(wine_data, aes(price, points, color = point_cat)) + geom_point() + theme_fivethirtyeight() + labs(title = "Score vs Price") + xlab("Points (0-100)") +
  ylab("Price ($)")

.save_pdf("price_v_points")

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
