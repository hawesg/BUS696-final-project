##### Designation START

library('tidyverse')
library('here')
library('ggthemes')
library('stringr')

wine_data <-
  read.csv(here("data", "input", "winemag-data-130k-v2.csv"))



unwanted_array  = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )


wine_data_Reduced <- wine_data %>%
  #filter(grepl("serv", designation, ignore.case=TRUE)) %>%
  mutate(designation=tolower(designation)) %>%
  mutate(designation=str_replace(designation,"[\\d¡\\;ã\\-£±ºÃª©]+","")) %>%
  #mutate(designation=str_replace(designation,"\\W+"," ")) %>%
  #mutate(designation=gsub("[\\d¡\\;ã\\-£±ºÃª©]+","",designation)) %>%
  mutate(designation=gsub("\\W+"," ",designation)) %>%
  mutate(designation=str_replace(designation,"[ãÃ©\\w]+ser[ãÃ©\\w]+","Reserve")) %>% 
  mutate(designation=chartr(paste(names(unwanted_array), collapse=''),paste(unwanted_array, collapse=''),designation)) %>% 
  mutate(designation=ifelse(grepl("Reserve",designation),"Reserve",designation)) %>% 
  mutate(designation=ifelse(grepl("extra dry",designation),"Extra Dry",ifelse(grepl("(dry|trocken)",designation),"Dry",designation))) %>% 
  mutate(designation=ifelse(grepl("brut",designation),"Brut",designation)) %>% 
  mutate(designation=ifelse(grepl("(estate|grand|casa)",designation),"Estate",designation)) %>% 
  mutate(designation=ifelse(grepl("single",designation),"Single Vineyard",designation)) %>% 
  mutate(designation=ifelse(grepl("(klassik|classic|tradition|vintage)",designation),"Classic Vintage",designation)) %>% 
  mutate(designation=ifelse(grepl("ros",designation),"Rose",designation)) %>% 
  mutate(designation=ifelse(grepl("barrel s",designation),"Barrel Sample",designation)) %>% 
  mutate(designation=ifelse(grepl("(old v|vieilles)",designation),"Old Vine",designation)) %>% 
  mutate(designation=ifelse(grepl("(vineyard|ranch|alpha|branco|broquel)",designation),"Some Vineyard",designation)) %>% 
  mutate(designation=ifelse(grepl("(barrel|crianza|cuve)",designation),"Barrel",designation)) %>% 
  mutate(designation=ifelse(grepl("unoaked",designation),"UnOaked",designation)) %>% 
  mutate(designation=ifelse(grepl("cuve prestige",designation),"Finest Champagne",designation)) %>% 
  mutate(designation=ifelse(grepl("(blanc|white|bianco)",designation),"White",designation)) %>% 
  mutate(designation=ifelse(grepl("(red|tinto|bussia)",designation),"Red",designation)) %>% 
  mutate(designation=ifelse(grepl("(nouveau|proprietary|signature|selec|premier)",designation),"Signature",designation)) %>% 
  mutate(designation=ifelse(grepl("lot",designation),"Lot",designation)) %>% 
  mutate(designation=ifelse(grepl("late",designation),"Late Harvest",designation)) %>% 
  mutate(designation=ifelse(grepl("(oak|roble)",designation),"Oak",designation)) %>% 
  mutate(designation=ifelse(grepl("(organic|cannubi)",designation),"Organic",designation)) %>% 
  mutate(designation=ifelse(grepl("(port|colheita)",designation),"Port",designation)) %>% 
  mutate(designation=ifelse(grepl("(collection|premium|prestige|limited)",designation),"Premium",designation)) %>% 
  mutate(designation=ifelse(grepl("clone",designation),"Clone",designation)) %>% 
  mutate(designation=ifelse(grepl("(block|bin)",designation),"Block",designation)) 
  #select(designation) %>%
  #unique() %>% 
  #View()

  summary(wine_data_Reduced)
dim(wine_data_Reduced)
    
wine_data_Reduced <- wine_data_Reduced %>% 
  group_by(designation) %>% 
  summarize(count_obs=n()) %>% 
  filter(count_obs > 1) %>% 
  arrange (desc(count_obs)) %>% 
  slice(1:25)



summary(wine_data_Reduced)
dim(wine_data_Reduced)


ggplot(wine_data_Reduced,aes(x=designation,y=count_obs)) + geom_col() +
  labs(x = "designation", 
       y = "count") +
  ggtitle("designation") +
  theme(axis.text.x = element_text(angle=270))








#### Designation END








################### Feature Generation ##################

# TODO Decide on factors for price groups budget, ... premium, ultra premium? then use cut to devide them into buckets.

# Turn score into categories per https://www.winespectator.com/articles/scoring-scale
wine_data_stats <-
  wine_data  %>% mutate (point_cat = cut(
    wine_data$points,
    breaks = c(0, 73, 76, 79, 82, 85, 88, 91, 94, 97, 100),
    labels = c(
      "10.Not Recomended",
      "9.Should Avoid",
      "8.Not so Good",
      "7.Mediocre",
      "6.Not So Bad",
      "5.Good",
      "4.Very good",
      "3.Outstanding",
      "2.Classic",
      "1.F.Awesome"
    )
  )) %>% 
  group_by(point_cat) %>% 
  summarize(count_obs=n()) %>% 
  filter(count_obs > 1)


head(wine_data_stats)

ggplot(wine_data_stats,aes(x=point_cat,y=count_obs)) + geom_col() +
  labs(x = "recomendation scale", 
       y = "count") +
  ggtitle("recomendation variation") +
  theme(axis.text.x = element_text(angle=270))


# TODO maybe change levels to ordered and also drop unused ones

# Length of title
wine_data <-
  wine_data %>% mutate (title_length = nchar(as.character(wine_data$title)))

# Add a column to indicate wheather the wine includes a vintage # might also use str_extract to get that vintage
wine_data <-
  wine_data  %>% mutate (includes_vintage = grepl("(19\\d{2}|20\\d{2})", title))

head(wine_data)

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
