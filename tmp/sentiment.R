#-------------------------------------------------------------------------------
# Text Sentiment
#-------------------------------------------------------------------------------
library('devtools')
devtools::install_github("trinker/sentimentr")


# average sentiment score
library('sentimentr')
library('tidyverse')

wine_data <- 
  wine_data %>% dplyr::mutate(title_no_accents = stringi::stri_trans_general(title, "Latin-ASCII"))

sentiment_by(wine_short$title_no_accents)

sentiment(wine_short$title_no_accents)

s <- sentiment_by(wine_data$title_no_accents)

summary(s)
str(s)

# 
# sentiment_b_s <- sentiment_by(wine_short$title_no_accents)
# sentiment_b_s <- sentiment_b_s$ave_sentiment
#   
#   
# sentiment_s <- sentiment(wine_short$title_no_accents)
# sentiment_s <- sentiment_s$sentiment



wine_data <- wine_data %>% dplyr::mutate( title_sentement = s$ave_sentiment, title_word_count = s$word_count)

