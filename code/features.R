#************* Feature Generation *****************

# TODO Decide on factors for price groups budget, ... premium, ultra premium? 
# then use cut to devide them into buckets. Or not since we are modeling price.



# Points Categories -------------------------------------------------------



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

# TODO maybe change levels to ordered and also drop unused facttors

################### Title Features ##################

# Title Length ------------------------------------------------------------


wine_data <-
  wine_data %>% mutate (title_length = nchar(as.character(wine_data$title)))


# Title Includes Date -----------------------------------------------------

# Add a column to indicate wheather the wine includes a date in the tile 

wine_data <-
  wine_data  %>% mutate (has_year_in_title = grepl("(19|20)\\d{2})", title))

# check regex to see if results are the same length 

# dim(wine_data  %>% mutate (has_year_in_title = grepl("(19\\d{2}|20\\d{2})", title)))
# 
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
  wine_data %>% mutate(title_has_accents = (title != stringi::stri_trans_general(title, "Latin-ASCII") ) )

################### Designation column  ##################

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
    ".*(block|bin).*"
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
    "Block"
  )

# This is so that we can use the replacements object in str_replace_all rather than a single pattern/replacement
names(replacements) <- patterns


wine_designations_no_accents <-
  wine_data %>% mutate(designation = stringi::stri_trans_general(designation, "Latin-ASCII")) %>% # translate from accented charecters to their non accented counterparts,
  mutate(designation = tolower(str_squish(
    str_replace_all(designation, "[^A-Za-z]", " ")
  ))) # get rid of all non letter charecters then trim white space and make lower case

wine_designations_no_accents <-
  wine_designations_no_accents %>% mutate(designation = str_replace_all(wine_designations_no_accents$designation, replacements)) %>% # replace per above
  mutate(designation = as.factor(designation))


summary(wine_designations_no_accents)

glimpse(wine_designations_no_accents)

dim(wine_designations_no_accents %>% filter(grepl("[¡]", designation)))
# 1
dim(wine_designations_no_accents %>% filter(grepl("[\\;]", designation)))
# 12
dim(wine_designations_no_accents %>% filter(grepl("[\\-]", designation)))
# 2037

# Find records that match one but not another

x <-
  wine_designations_no_accents %>% filter(grepl("r[ei]serv[ae]", designation))
y <-
  wine_designations_no_accents %>% filter(grepl("[ei]serv", designation))
setdiff(x, y)

glimpse(wine_data)
wine_data <- wine_designations_no_accents


# Reviewers ---------------------------------------------------------------

# Lump Factors

wine_data <-
  wine_data %>% filter(as.character(taster_twitter_handle) != "") %>% mutate(
    taster_name_lump = fct_lump(taster_name, n = 5),
    taster_twitter_lump = fct_lump(taster_twitter_handle, n = 5)
  )

# Add stats about each reviewer 

wine_data <- wine_data %>% 
  group_by(taster_twitter_handle) %>% 
  mutate(taster_avg_points = mean(points), taster_review_count= n())


# Add ID Column -----------------------------------------------------------

wine_data <- tibble::rowid_to_column(wine_data, "ID")

# Get list of column names in vector form

dput(colnames(wine_data))

glimpse(wine_data)

wine_data_clean <-
  wine_data %>% mutate
  select( 
    ID, 
    variety,
    country,
    designation,
    points,
    price,
    point_cat,
    title_length,
    has_year_in_title,
    title_has_accents,
    taster_twitter_lump,
    color_lump
  )

str(wine_data_clean)
glimpse(wine_data_clean)

#wine_data_with_twitter_data <- merge(wine_data, TwitterData, by = "taster_twitter_handle")

#wine <- merge(wine_data, twitter_stats, by = "taster_twitter_handle")

# TODO missing records


# levels(wine_data$taster_name)
#
# wine_data$taster_name
#
#
# # TODO do we drop the reviews that have no name assuming that they should have a reviewer and this is erronious data?
#
# cf <- fct_lump(wine_data$taster_name, n = 5)
# levels(cf)
# ? fct_lump
