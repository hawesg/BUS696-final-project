

################### Feature Generation ##################

# TODO Decide on factors for price groups budget, ... premium, ultra premium? then use cut to devide them into buckets.

# Turn score into categories per https://www.winespectator.com/articles/scoring-scale
wine_data <-
  wine_data  %>% mutate (point_cat = cut(
    wine_data$points,
    breaks = c(0, 74, 79, 84, 89, 94, 100),
    labels = c(
      "Not Recomended",
      "Mediocre",
      "Good",
      "Very good",
      "Outstanding",
      "Classic"
    )
  ))

# TODO maybe change levels to ordered and also drop unused ones

# Length of title
wine_data <-
  wine_data %>% mutate (title_length = nchar(as.character(wine_data$title)))

# Add a column to indicate wheather the wine includes a vintage # might also use str_extract to get that vintage
wine_data <-
  wine_data  %>% mutate (includes_vintage = grepl("(19\\d{2}|20\\d{2})", title))

# Add a column to indicate wheather the wine includes some variation of reserve
wine_data <-
  wine_data  %>% mutate (is_reserve = grepl("[Rr][ei]serv[ea]", designation))


# TODO This is not quite right because there is still a few where they have included a year that is not the vintage
#vintage = str_extract(title, "(19\\d{2}|20\\d{2})") )

# TODO Maybe a regex for if the title has accents in it (ie seems forign and fancy)

# Get Names Vector
dput(colnames(wine_data))

levels(wine_data$taster_name)

c(
  "X",
  "country",
  "description",
  "designation",
  "points",
  "price",
  "province",
  "region_1",
  "region_2",
  "taster_name",
  "taster_twitter_handle",
  "title",
  "variety",
  "winery",
  "vintage",
  "includes_vintage",
  "point_cat",
  "title_length",
  "is_reserve"
)

cf <- fct_lump(wine_data$taster_name, n = 5)
levels(cf)
? fct_lump
