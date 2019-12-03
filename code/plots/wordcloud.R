

##Load Require Library
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
##Read the Data

library(tidyverse)
library(stringi)
library(stringr)


# wine_designations_word_cloud <- wine_designations_word_cloud$designation
wine_designations_word_cloud <- str_replace_all(wine_designations_word_cloud, "([Rr].serv.)", "Reserve")


# wine_designations_word_cloud <-
#   wine_designations_word_cloud %>% dplyr::mutate(designation = str_replace_all(designation, "([Rr].serv.)", "Reserve"))

## Calculate Corpus

wineDesignation.Corpus <-
  Corpus(VectorSource(wine_designations_word_cloud))
  # Corpus(VectorSource(wine_designations_word_cloud$designation))

##Data Cleaning and Wrangling

wineDesignation.clean <- tm_map(wineDesignation.Corpus,
                                PlainTextDocument)
wineDesignation.clean <-
  tm_map(wineDesignation.Corpus, tolower)
wineDesignation.clean <-
  tm_map(wineDesignation.Corpus, removeNumbers)
wineDesignation.clean <-
  tm_map(wineDesignation.Corpus, removeWords, stopwords("english"))
wineDesignation.clean <-
  tm_map(wineDesignation.Corpus, removePunctuation)
wineDesignation.clean <-
  tm_map(wineDesignation.Corpus, stripWhitespace)
# wineDesignation.clean <-
#   tm_map(wineDesignation.Corpus, stemDocument)

# Colors

# scale: Indicates the range of sizes of the words.
# max.words: Plots the specified number of words and discard least frequent terms.]
# min.freq: Discards all terms whose frequency is below the specified value.
# random.order: By setting this to false, we make it so that the words with the highest frequency are plotted first. If we don’t set this, it will plot the words in a random order and the highest frequency words may not necessarily appear in the center.
# rot.per: Determines the fraction of words that are plotted vertically.
# colors: The default value is black. If you want to use different colors based on frequency, you can specify a vector of colors or use one of the pre-defined color palettes.

wordcloud(
  words = wineDesignation.clean,
  min.freq = 2,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)
