load(here::here("data","output","clean_wine.RData"))
world_map <- map_data("world")
world_map <-
  world_map %>% dplyr::mutate(region = ifelse(region == "USA", "US", region))
world_map <- world_map %>% dplyr::mutate(country = region)

wine_map_DF <-
  wine_data_clean  %>% dplyr::mutate(country = as.character(country)) %>%
  dplyr::filter (country != "England" &
            country != "US-France" &
            country != "")
glimpse(wine_map_DF)

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
    price_max = max(price, na.rm = TRUE)
  ) %>%
  dplyr::select(
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
      scale_fill_viridis_c(option = "C") + theme_fivethirtyeight() + ggtitle(title)
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