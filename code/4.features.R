#************* Feature Generation *****************

#Helper functions
source("code/helper_functions/features_fxns.R")

# Points Categories -------------------------------------------------------

# Turn score into categories per https://www.winespectator.com/articles/scoring-scale

wine_data <-
  wine_data  %>% dplyr::mutate(point_cat = cut(
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

# TODO maybe change levels to ordered

################### Title Features ##################

# Title Length ------------------------------------------------------------

wine_data <-
  wine_data %>%
  dplyr::mutate(title_length = nchar(as.character(title)))


wd_a_tl <- wine_data

wine_data <- wd_a_tl

# Title Includes Date -----------------------------------------------------

# Add a column to indicate wheather the wine includes a date in the tile

# wine_data  %>% filter( grepl("(19|20)\\d{2})", title) )
#
# wine_data <-
#   wine_data  %>% mutate (has_year_in_title = grepl("(19|20)\\d{2})", title))

# check regex to see if results are the same length

# dim(wine_data  %>% mutate (has_year_in_title = grepl("(19\\d{2}|20\\d{2})", title)))
# #
# dim(wine_data  %>% mutate (has_year_in_title = grepl("(19|20)\\d{2})", title)))



# Title has Accent(s) -----------------------------------------------------
# Has accents presuming the american market might pay more ... or less for wines that sound exotic.

wine_data <-
  wine_data %>%
  dplyr::mutate(title_no_accents = stringi::stri_trans_general(title, "Latin-ASCII")) %>% 
  dplyr::mutate(title_has_accents = as.numeric(title != title_no_accents))

# Title Sentiment Analysis and Word Count ---------------------------------

s <- sentiment_by(wine_data$title_no_accents)

#qplot(s$ave_sentiment, geom="histogram",binwidth=0.2,main="Wine Title Sentiment Histogram")

wine_data <-
  wine_data %>% dplyr::mutate(
    title_sentement = s$ave_sentiment,
    title_word_count = s$word_count
  )

################### Designation column  ##################

# wine_designations_no_accents <-
#   wine_data %>% mutate(designation = stringi::stri_trans_general(designation, "Latin-ASCII")) %>% # translate from accented charecters to their non accented counterparts,
#   mutate(designation = tolower(str_squish(
#     str_replace_all(designation, "[^A-Za-z]", " ")
#   ))) # get rid of all non letter charecters then trim white space and make lower case

wdx <- wine_data

# wine_data <- wine_data %>%
#   dplyr::mutate(designation = stringi::stri_trans_general(designation, "Latin-ASCII"))
wine_data <- wine_data %>%
  dplyr::mutate(designation = .clean_text(designation))

wine_designations_word_cloud <- wine_data$designation

###### USE WORDCLOUD.R TO NARROW DOWN POTENTIAL FACTORS

# List of regular expressions to match

#levels(factor(wine_data$designation))

patterns <-
  c(
    ".*([Rr].serv).*",
    ".*extra dry.*",
    ".*(dry|trocken).*",
    ".*brut.*",
    ".*(estate|grand|casa).*",
    ".*single.*",
    ".*(klassik|classic|tradition|vintage).*",
    ".*rose.*",
    ".*barrel s.*",
    ".*(old v|vieilles).*",
    ".*(vineyard|ranch|alpha|branco|broquel).*",
    ".*(barrel|crianza|cuve).*",
    ".*unoaked.*",
    ".*cuve prestige.*",
    ".*(blanc|white|bianco).*",
    ".*(red|tinto|bussia).*",
    ".*(nouveau|proprietary|signature|selec|premier).*",
    ".*lot.*",
    ".*late.*",
    ".*(oak|roble).*",
    ".*(organic|cannubi).*",
    ".*(port|colheita).*",
    ".*(collection|premium|prestige|limited).*",
    ".*clone.*",
    ".*(block|bin).*",
    "^$"
  )

# Values to replace above patterns with

replacements <-
  c(
    "Reserve",
    "Extra Dry",
    "Dry",
    "Brut",
    "Estate",
    "Single Vineyard",
    "Classic Vintage",
    "Rose",
    "Barrel Sample",
    "Old Vine",
    "Some Vineyard",
    "Barrel",
    "UnOaked",
    "Finest Champagne",
    "White",
    "Red",
    "Signature",
    "Lot",
    "Late Harvest",
    "Oak",
    "Organic",
    "Port",
    "Premium",
    "Clone",
    "Block",
    "No Designation"
  )

# This is so that we can use the replacements object in str_replace_all rather than a single pattern/replacement
# dput(replacements)
names(replacements) <- patterns
# wine_data <-
#   wine_data %>%
#   dplyr::mutate(designation = stringr::str_replace_all(wine_data$designation, replacements)) %>% # replace per above
#   dplyr::mutate(designation = as.factor(designation))

designation_rp <-
  stringr::str_replace_all(wine_data$designation, replacements)

wine_data$designation <- as.factor(designation_rp)

# Lump Factors ---------------------------------------------------------------

wine_data <-
  wine_data %>% mutate(
    taster_name = as_factor(taster_name),
    taster_twitter_handle = as_factor(taster_twitter_handle),
    variety = as_factor(variety)
  )

.number_of_factor_levels_containing_num_observations <-
  function(fct, num) {
    x <- nrow(fct_count(fct, sort = TRUE) %>% dplyr::filter(n >= num))
    return(x)
  }

if (FCT_LUMPS$by_count != 0) {
  fct_x <- .number_of_factor_levels_containing_num_observations
  FCT_LUMPS[['taster_name']] <-
    fct_x(wine_data$taster_name, FCT_LUMPS$by_count)
  FCT_LUMPS[['taster_twitter']] <-
    fct_x(wine_data$taster_twitter_handle, FCT_LUMPS$by_count)
  FCT_LUMPS[['country']] <-
    fct_x(wine_data$country, FCT_LUMPS$by_count)
  FCT_LUMPS[['variety']] <-
    fct_x(wine_data$variety, FCT_LUMPS$by_count)
  FCT_LUMPS[['province']] <-
    fct_x(wine_data$province, FCT_LUMPS$by_count)
  FCT_LUMPS[['winery']] <-
    fct_x(wine_data$winery, FCT_LUMPS$by_count)
  #Red and white variety-color
  r <- wine_data %>% dplyr::filter(color_lump == "Red")
  w <- wine_data %>% dplyr::filter(color_lump == "White")
  FCT_LUMPS[['variety.red']] <- fct_x(r$variety, FCT_LUMPS$by_count)
  FCT_LUMPS[['variety.white']] <-
    fct_x(w$variety, FCT_LUMPS$by_count)
  rm(fct_x, r, w)
}

wine_data <-
  wine_data %>%
  dplyr::mutate(
    taster_name_lump = fct_lump(taster_name, n = FCT_LUMPS$taster_name),
    taster_twitter_lump = fct_lump(taster_twitter_handle, n = FCT_LUMPS$taster_twitter),
    designation_lump = fct_lump(designation, n = FCT_LUMPS$designation),
    country_lump = fct_lump(country, n = FCT_LUMPS$country),
    variety_lump = fct_lump(variety, n = FCT_LUMPS$variety),
    province_lump = fct_lump(province, n = FCT_LUMPS$province),
    color_lump = fct_lump(color, n = 2),
    winery_lump = fct_lump(winery, n = FCT_LUMPS$winery),
    province_lump = fct_lump(wine_data$province, n = FCT_LUMPS$province),
  )

# Lump Varieties ----------------------------------------------------------

.append_color_to_factor <- function(var, col) {
  var <- as.character(var)
  var <- paste(var, col)
  return(var)
}

# Lump Variety and Color together -----------------------------------------

red <-
  wine_data %>% dplyr::filter(color_lump == "Red") %>%
  dplyr::mutate(variety_color = fct_lump(variety, n = FCT_LUMPS$variety.red)) %>%
  dplyr::mutate(variety_color = .append_color_to_factor(variety_lump, "(R)"))
white <-
  wine_data %>% dplyr::filter(color_lump == "White") %>%
  dplyr::mutate(variety_color = fct_lump(variety, n = FCT_LUMPS$variety.white)) %>%
  dplyr::mutate(variety_color = .append_color_to_factor(variety_lump, "(W)"))
other <-
  wine_data %>% dplyr::filter(color_lump == "Other") %>%
  dplyr::mutate(variety_color = "Other")

if (FCT_LUMPS$by_count != 0) {
  fct_x <- .number_of_factor_levels_containing_num_observations
  FCT_LUMPS[['variety.red']] <-
    fct_x(wine_data$province, FCT_LUMPS$by_count)
  FCT_LUMPS[['variety.white']] <-
    fct_x(wine_data$winery, FCT_LUMPS$by_count)
  rm(fct_x)
}

wine_data_bind <- do.call("rbind", list(red, white, other))

wine_data <-
  wine_data_bind %>% dplyr::mutate(variety_color = as_factor(variety_color))


# Add summary info per taster ----------------------------------

wine_data <- wine_data %>%
  dplyr::group_by(taster_name) %>%
  dplyr::mutate(taster_avg_points = mean(points),
                taster_review_count = n()) %>% dplyr::ungroup()

# Add ID Column -----------------------------------------------------------

wine_data <- tibble::rowid_to_column(wine_data, "ID")

wine_data <- wine_data %>% droplevels()

wine_data <-
  wine_data %>% dplyr::mutate(variety = as_factor(variety))

wine_data_clean <-
  wine_data %>%
  dplyr::select(
    # ID,
    price,
    points,
    point_cat,
    country,
    country_lump,
    province_lump,
    winery_lump,
    color_lump,
    variety_lump,
    variety_color,
    designation_lump,
    title_word_count,
    title_sentement,
    title_length,
    title_has_accents,
    taster_name_lump,
    taster_twitter_lump,
    taster_gender,
    taster_avg_points,
    taster_review_count,
    taster_n_tweets,
    taster_n_followers,
  ) %>% droplevels()

setdiff(names(wine_data), names(wine_data_clean))


# Rename columns ----------------------------------------------------------

wd_temp <- wine_data_clean

colnames(wd_temp) <-
  c(
    "price",
    "points",
    "points.category",
    "country.map",
    "country",
    "province",
    "winery",
    "color",
    "variety",
    "variety_and_color",
    "designation",
    "title.n_words",
    "title.sentement",
    "title.n_chars",
    "title.has_accents",
    "taster.name",
    "taster.twitter_handle",
    "taster.gender",
    "taster.avg_points",
    "taster.n_reviews",
    "taster.n_tweets",
    "taster.n_followers"
  )
wine_data_clean <- wd_temp
