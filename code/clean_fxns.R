# rollback wine data if there is a change that you don't like

.rollback_wine_date <- function() {
  wine_data <- wine_data_orignal
}

.clean_text <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_replace_all(x, "[^A-Za-z]", " ")
  x <- tolower(x)
  x <- str_squish(x)
  return(x)
}