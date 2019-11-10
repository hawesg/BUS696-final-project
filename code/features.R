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

# TODO maybe change levels to ordered and also drop unused facttors

# Length of title
wine_data <-
  wine_data %>% mutate (title_length = nchar(as.character(wine_data$title)))

# Add a column to indicate wheather the wine includes a vintage # might also use str_extract to get that vintage

# TODO this isn't quite right 2 options someone goes through the following 100K+ by hand or we call the variable title_has_year

wine_data %>% filter(grepl("(19\\d{2}|20\\d{2})", title)) %>%
  select(title) 
               
wine_data <-
  wine_data  %>% mutate (has_year_in_title = grepl("(19\\d{2}|20\\d{2})", title))

##### Designation column

# List of regular expressions to match

patterns <- c(".*([Rr].serv).*", ".*extra dry.*", ".*(dry|trocken).*", ".*brut.*", ".*(estate|grand|casa).*", 
              ".*single.*", ".*(klassik|classic|tradition|vintage).*", ".*rose.*", ".*barrel s.*", ".*(old v|vieilles).*", 
              ".*(vineyard|ranch|alpha|branco|broquel).*", ".*(barrel|crianza|cuve).*", ".*unoaked.*", ".*cuve prestige.*", 
              ".*(blanc|white|bianco).*", ".*(red|tinto|bussia).*", ".*(nouveau|proprietary|signature|selec|premier).*", 
              ".*lot.*", ".*late.*", ".*(oak|roble).*", ".*(organic|cannubi).*", ".*(port|colheita).*", 
              ".*(collection|premium|prestige|limited).*", ".*clone.*",  ".*(block|bin).*")

# Values to replace above patterns with

replacements <- c("Reserve", "Extra Dry", "Dry", "Brut", "Estate", "Single Vineyard", "Classic Vintage",  "Rose",  
                  "Barrel Sample", "Old Vine", "Some Vineyard", "Barrel", "UnOaked", "Finest Champagne", "White", 
                  "Red", "Signature", "Lot", "Late Harvest", "Oak", "Organic", "Port", "Premium", "Clone", "Block")

# This is so that we can use the replacements object in str_replace_all rather than a single pattern/replacement
names(replacements) <- patterns



wine_designations_no_accents <-
  wine_data %>% mutate(designation = stringi::stri_trans_general(designation, "Latin-ASCII")) %>% # translate from accented charecters to their non accented counterparts,
  mutate(designation = tolower(str_squish(str_replace_all(designation, "[^A-Za-z]", " ")))) %>% # get rid of all non letter charecters then trim white space and make lower case
  mutate(designation = str_replace_all(wine_designations_no_accents$designation, replacements) ) %>% # replace per above
  mutate(designation = as.factor(designation)) 

summary(wine_designations_no_accents)

ggplot(wine_designations_no_accents, aes(x=designation)) + geom_bar()



View(wine_designations_no_accents)

dim(wine_designations_no_accents %>% filter(grepl("[¡]", designation)))
# 1
dim(wine_designations_no_accents %>% filter(grepl("[\\;]", designation)))
# 12
dim(wine_designations_no_accents %>% filter(grepl("[\\-]", designation)))
# 2037

# Find records that match one but not another

x <- wine_designations_no_accents %>% filter(grepl("r[ei]serv[ae]", designation))
y <- wine_designations_no_accents %>% filter(grepl("[ei]serv", designation)) 
setdiff(x,y)

# TODO Maybe a regex for if the title has accents in it (ie seems forign and fancy)





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
