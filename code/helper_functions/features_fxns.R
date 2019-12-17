.tukey <- function(p){
  p <- p ^ (-.3)
  return(-p)
}

.append_color_to_factor <- function(variety, color) {
  variety = as.character(variety)
  variety = paste(variety, color)
  return(variety)
}
 