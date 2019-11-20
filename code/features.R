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

# TODO maybe change levels to ordered and also drop unused facttors

################### Title Features ##################

# Title Length ------------------------------------------------------------

wine_data <-
  wine_data %>% dplyr::mutate(title_length = nchar(as.character(wine_data$title)))


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

# title_tibble <-
#   tibble(wine_data$title,
#          stringi::stri_trans_general(wine_data$title, "Latin-ASCII"))
# names(title_tibble) <- c("t1", "t2")
# names(title_tibble)
# title_tibble <- title_tibble %>% mutate(title_has_accent = (t1 != t2))

wine_data <-
  wine_data %>% dplyr::mutate(title_has_accents = (title != stringi::stri_trans_general(title, "Latin-ASCII")))



# Title Sentiment Analysis and Word Count ---------------------------------

wine_data <- 
  wine_data %>% dplyr::mutate(title_no_accents = stringi::stri_trans_general(title, "Latin-ASCII"))

s <- sentiment_by(wine_data$title_no_accents)

qplot(s$ave_sentiment, geom="histogram",binwidth=0.2,main="Wine Title Sentiment Histogram")

wine_data <- wine_data %>% dplyr::mutate( title_sentement = s$ave_sentiment, title_word_count = s$word_count)

################### Designation column  ##################

# wine_designations_no_accents <-
#   wine_data %>% mutate(designation = stringi::stri_trans_general(designation, "Latin-ASCII")) %>% # translate from accented charecters to their non accented counterparts,
#   mutate(designation = tolower(str_squish(
#     str_replace_all(designation, "[^A-Za-z]", " ")
#   ))) # get rid of all non letter charecters then trim white space and make lower case

wine_data <- wine_data %>%
  dplyr::mutate(designation = .clean_text(designation))

wine_designations_word_cloud <- wine_data

###### USE WORDCLOUD.R TO NARROW DOWN POTENTIAL FACTORS

# List of regular expressions to match

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

names(replacements) <- patterns

wine_data <-
  wine_data %>% dplyr::mutate(designation = str_replace_all(designation, replacements)) %>% # replace per above
  dplyr::mutate(designation = as.factor(designation))


summary(wine_data)

glimpse(wine_data)

dim(wine_data %>% dplyr::filter(grepl("[¡]", designation)))
# 1
dim(wine_data %>% dplyr::filter(grepl("[\\;]", designation)))
# 12
dim(wine_data %>% dplyr::filter(grepl("[\\-]", designation)))
# 2037

# Find records that match one but not another

x <-
  wine_data %>% dplyr::filter(grepl("r[ei]serv[ae]", designation))
y <-
  wine_data %>% dplyr::filter(grepl("[ei]serv", designation))
setdiff(x, y)

glimpse(wine_data)

wd_a_des <- wine_data

wine_data <- wd_a_des

# Lump Varieties ----------------------------------------------------------

.append_color_to_factor <- function(variety, color) {
  variety = as.character(variety)
  variety = paste(variety, color)
  return(variety)
}

head(table(wine_data$variety, wine_data$color_lump))

VARIETY_PER_COLOR_LUMP <- 5

red <-
  wine_data %>% dplyr::filter(color_lump == "Red") %>%
  dplyr::mutate(variety_lump = fct_lump(variety, n = VARIETY_PER_COLOR_LUMP)) %>%
  dplyr::mutate(variety_lump = .append_color_to_factor(variety_lump, "(R)"))
white <-
  wine_data %>% dplyr::filter(color_lump == "White") %>%
  dplyr::mutate(variety_lump = fct_lump(variety, n = VARIETY_PER_COLOR_LUMP)) %>%
  dplyr::mutate(variety_lump = .append_color_to_factor(variety_lump, "(W)"))
other <-
  wine_data %>% dplyr::filter(color_lump == "Other") %>%
  dplyr::mutate(variety_lump = "Other")

wine_data_bind <- do.call("rbind", list(red, white, other))

wine_data <-
  wine_data_bind %>% dplyr::mutate(variety_lump = factor(variety_lump))



glimpse(wine_data_bind)
summary(wine_data)
levels(wine_data$variety_lump)

# temp_fct <- factor(red$variety_lump)
# unique(other$variety_lump)
# glimpse(red)

# white$variety_lump %>% fct_count()
# glimpse(white)
# summary(red)
# levels(white$variety_lump)

# Lump Factors ---------------------------------------------------------------

# TODO look up constants and replace these with variables

wine_data <-
  wine_data %>% dplyr::filter(as.character(taster_twitter_handle) != "") %>% dplyr::mutate(
    taster_name_lump = fct_lump(taster_name, n = TASTER_NAME_LUMP),
    taster_twitter_lump = fct_lump(taster_twitter_handle, n = TASTER_TWITTER_LUMP),
    designation_lump = fct_lump(designation, n = DESIGNATION_LUMP),
    country_lump = fct_lump(country, n = COUNTRY_LUMP),
    variety_lump = fct_lump(variety, n = VARIETY_LUMP)
  )

glimpse(wine_data)

summary(wine_data)

str(wine_data)

# Add stats about each reviewer

#mutate(taster_name = factor(taster_name)) %>%

wine_data <- wine_data %>%
  dplyr::group_by(taster_name) %>%
  dplyr::mutate(taster_avg_points = mean(points),
                   taster_review_count = n()) %>% dplyr::ungroup()

# summarize(taster_avg_points = mean(points),
#      taster_review_count = 1000 )

glimpse(wine_data)

# Add ID Column -----------------------------------------------------------

wine_data <- tibble::rowid_to_column(wine_data, "ID")

names(wine_data)



# Drop unused point cat factors -------------------------------------------
setdiff(levels(wine_data$point_cat), wine_data$point_cat)
fct_drop(wine_data$point_cat)

# Get list of column names in vector form

dput(colnames(wine_data))

names(wine_data)

glimpse(wine_data)

summary(wine_data)



# .bart <- function(x){
#   taster_n_tweets - median(wine_data_clean$taster_n_tweets))/(max(wine_data_clean$taster_n_tweets) - min(wine_data_clean$taster_n_tweets)
# }
# Bart Mutate -------------------------------------------------------------

wine_data <- wine_data %>% dplyr::mutate( taster_n_tweets_per = (taster_n_tweets - median(wine_data_clean$taster_n_tweets))/(max(wine_data_clean$taster_n_tweets) - min(wine_data_clean$taster_n_tweets)),
                                title_word_count_per = (title_word_count - median(wine_data_clean$title_word_count))/(max(wine_data_clean$title_word_count) - min(wine_data_clean$title_word_count)),
                                taster_review_count_per = (taster_review_count - median(wine_data_clean$taster_review_count))/(max(wine_data_clean$taster_review_count) - min(wine_data_clean$taster_review_count)),
                                taster_avg_points_per = (taster_avg_points - median(wine_data_clean$taster_avg_points))/(max(wine_data_clean$taster_avg_points) - min(wine_data_clean$taster_avg_points)))
                               

# TODO Add on to this
wine_data_clean <-
  wine_data %>%
  dplyr::select(
    ID,
    price,
    country,
    variety,
    points,
    point_cat,
    title_length,
    title_has_accents,
    variety_lump,
    designation_lump,
    taster_name_lump,
    taster_twitter_lump,
    taster_gender,
    taster_avg_points,
    taster_review_count,
    taster_n_tweets,
    color_lump,
    country_lump,
    province_lump,
    title_word_count,
    title_sentement,
    taster_n_tweets_per,
    title_word_count_per,
    taster_review_count_per,
    taster_avg_points_per
  ) %>% droplevels()

setdiff(names(wine_data), names(wine_data_clean))

glimpse(wine_data_clean)

str(wine_data_clean)

summary(wine_data_clean)

head(wine_data)




