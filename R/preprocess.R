#' Data values can be scaled into the range of [0, 1] which is called normalization.
#'
dm_normalise <- function(x){
  (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
}

#' Scale transform calculates the standard deviation for an attribute 
#' and divides each value by that standard deviation
dm_scale <- function(x){
  (x - mean(x)) / sd(x)
 }
