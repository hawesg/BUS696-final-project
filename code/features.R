#************* Feature Generation *****************

# TODO Decide on factors for price groups budget, ... premium, ultra premium?
# then use cut to devide them into buckets. Or not since we are modeling price.

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


# TODO MAYBE AS NUMERIC also combine the logic for the next two

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

wine_designations_word_cloud <- wine_data

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

# summary(wine_data)
#
# glimpse(wine_data)
#
# dim(wine_data %>% dplyr::filter(grepl("[¡]", designation)))
# # 1
# dim(wine_data %>% dplyr::filter(grepl("[\\;]", designation)))
# # 12
# dim(wine_data %>% dplyr::filter(grepl("[\\-]", designation)))
# # 2037
#
# # Find records that match one but not another
#
# x <-
#   wine_data %>% dplyr::filter(grepl("r[ei]serv[ae]", designation))
# y <-
#   wine_data %>% dplyr::filter(grepl("[ei]serv", designation))
# setdiff(x, y)
#
# glimpse(wine_data)

# wd_a_des <- wine_data
#
# wine_data <- wd_a_des

# FCT_LUMPS <-
#   list(
#     taster_name = 13,
#     taster_twitter = 13,
#     designation = 25,
#     country = 17,
#     variety = 109,
#     variety.red = 56,
#     variety.white = 45,
#     province = 100,
#     winery = 166
#   )

temp <- wine_data

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
# List of 12
# $ by_count      : num 100
# $ taster_name   : int 14
# $ taster_twitter: int 13
# $ designation   : num 15
# $ country       : int 18
# $ variety       : int 75
# $ variety.red   : int 73
# $ variety.white : int 15
# $ variety.other : num 5
# $ province      : int 73
# $ winery        : int 15
# $ count_by      : num 100
# Lump Varieties ----------------------------------------------------------

.append_color_to_factor <- function(var, col) {
  var <- as.character(var)
  var <- paste(var, col)
  return(var)
}

# head(table(wine_data$variety, wine_data$color_lump))

# FCT_LUMPS <-
#   list(
#     variety_color = 5,
#     taster_name = 1,
#     taster_twitter = 5,
#     designation = 10,
#     country = 10,
#     variety = 10,
#     province = 10
#   )
# wine_data %>% dplyr::filter(color == "Other") %>% group_by(variety) %>%  summarise(count = n()) %>% View()



# FCT_LUMPS <-
#   list(
#     taster_name = 13,
#     taster_twitter = 13,
#     designation = 25,
#     country = 17,
#     variety = 109,
#     variety.red = 56,
#     variety.white = 45,
#     province = 100
#   )

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


# glimpse(wine_data_bind)
# summary(wine_data)
# levels(wine_data$variety_lump)

# temp_fct <- factor(red$variety_lump)
# unique(other$variety_lump)
# glimpse(red)

# white$variety_lump %>% fct_count()
# glimpse(white)
# summary(red)
# levels(white$variety_lump)


# glimpse(wine_data)
#
# summary(wine_data)
#
# str(wine_data)

# Add stats about each reviewer

#mutate(taster_name = factor(taster_name)) %>%

wine_data <- wine_data %>%
  dplyr::group_by(taster_name) %>%
  dplyr::mutate(taster_avg_points = mean(points),
                taster_review_count = n()) %>% dplyr::ungroup()

# summarize(taster_avg_points = mean(points),
#      taster_review_count = 1000 )

# glimpse(wine_data)

# Add ID Column -----------------------------------------------------------

wine_data <- tibble::rowid_to_column(wine_data, "ID")

# names(wine_data)



# Drop unused point cat factors -------------------------------------------
# setdiff(levels(wine_data$point_cat), wine_data$point_cat)
# fct_drop(wine_data$point_cat)

# Drop unused color factors -------------------------------------------
# fct_drop(wine_data$color)

wine_data <- wine_data %>% droplevels()

# dput(names(wine_data%>%select_if(is.factor)))
#
#
#
# nlevels(wine_data$country)
# nlevels(wine_data$description)
# nlevels(wine_data$designation)
# nlevels(wine_data$province)
# nlevels(wine_data$region_1)
# nlevels(wine_data$region_2)
# nlevels(wine_data$winery)
# nlevels(wine_data$color)
# nlevels(wine_data$taster_gender)
# nlevels(wine_data$color_lump)
# nlevels(wine_data$province_lump)
# nlevels(wine_data$country_lump)
# nlevels(wine_data$point_cat)
# nlevels(wine_data$variety_lump)
# nlevels(wine_data$taster_name_lump)
# nlevels(wine_data$taster_twitter_lump)
# nlevels(wine_data$designation_lump)
#
#
# levels.names <- c("country", "description", "designation", "province", "region_1",
#                   "region_2", "winery", "color", "taster_gender", "color_lump",
#                   "province_lump", "country_lump", "point_cat", "variety_lump",
#                   "taster_name_lump", "taster_twitter_lump", "designation_lump")
#
# levels.dl <- c(nlevels(wine_data_drop$country), nlevels(wine_data_drop$description), nlevels(wine_data_drop$designation), nlevels(wine_data_drop$province), nlevels(wine_data_drop$region_1), nlevels(wine_data_drop$region_2), nlevels(wine_data_drop$winery), nlevels(wine_data_drop$color), nlevels(wine_data_drop$taster_gender), nlevels(wine_data_drop$color_lump), nlevels(wine_data_drop$province_lump), nlevels(wine_data_drop$country_lump), nlevels(wine_data_drop$point_cat), nlevels(wine_data_drop$variety_lump), nlevels(wine_data_drop$taster_name_lump), nlevels(wine_data_drop$taster_twitter_lump), nlevels(wine_data_drop$designation_lump))
#
# levels.wd <- c(nlevels(wine_data$country), nlevels(wine_data$description)  , nlevels(wine_data$designation)  , nlevels(wine_data$province)  , nlevels(wine_data$region_1)  , nlevels(wine_data$region_2)  , nlevels(wine_data$winery)  , nlevels(wine_data$color)  , nlevels(wine_data$taster_gender)  , nlevels(wine_data$color_lump)  , nlevels(wine_data$province_lump) , nlevels(wine_data$country_lump) , nlevels(wine_data$point_cat), nlevels(wine_data$variety_lump), nlevels(wine_data$taster_name_lump) , nlevels(wine_data$taster_twitter_lump), nlevels(wine_data$designation_lump))
#
# levels.frame <- data.frame("column" = levels.names, "or" =levels.wd, "dl" = levels.dl)
# View(levels.frame)

# Get list of column names in vector form

#dput(colnames(wine_data))

# names(wine_data)
#
# glimpse(wine_data)
#
# summary(wine_data)
#

# TODO does this help?
# Bart Mutate -------------------------------------------------------------
.bart <- function(x) {
  return((x - median(x)) / diff(range(x)))
}
wine_data <-
  wine_data %>% dplyr::mutate(
    taster_n_tweets_per = .bart(taster_n_tweets),
    title_word_count_per = .bart(title_word_count),
    taster_review_count_per = .bart(taster_review_count),
    taster_avg_points_per = .bart(taster_avg_points),
    taster_n_followers_per  = .bart(taster_n_followers)
  )

wine_data <-
  wine_data %>% dplyr::mutate(variety = as_factor(variety))

# TODO Add on to this




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
    ###
    title_word_count_per,
    ####
    title_sentement,
    title_length,
    title_has_accents,
    taster_name_lump,
    taster_twitter_lump,
    taster_gender,
    taster_avg_points,
    ###
    taster_avg_points_per,
    ###
    taster_review_count,
    ###
    taster_review_count_per,
    ####
    taster_n_tweets,
    ###
    taster_n_tweets_per,
    ###
    taster_n_followers,
    ##
    taster_n_followers_per##
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
    "title.n_words_per",
    "title.sentement",
    "title.n_chars",
    "title.has_accents",
    "taster.name",
    "taster.twitter_handle",
    "taster.gender",
    "taster.avg_points",
    "taster.avg_points_per",
    "taster.n_reviews",
    "taster.n_reviews_per",
    "taster.n_tweets",
    "taster.n_tweets_per",
    "taster.n_followers",
    "taster.n_followers_per"
  )
wine_data_clean <- wd_temp

# dput(names(wine_data_clean))

# wine_data_clean_bart <- wine_data_clean



# glimpse(wine_data_clean)
#
# str(wine_data_clean)
#
# summary(wine_data_clean)
#
# head(wine_data)
