#' Types for type checking
#' @description This functions returns the types for a given dataset or 
#' field. 
#' 
#' @param x the data to return types for
#' 
#' @example types(dplyr::starwars)
#' 
types <- function(x) {
  sapply(x, class)
}

#' Distribution of data based off Categories in column
#' @description This functions works off categorical data. It
#' will return the frequency and percentage of data points
#' that fit to each distinct category. 
#' 
#' @example dist(dplyr::starwars$gender)
dist <- function(column) {
  cbind(freq=table(column), percentage=prop.table(table(column))*100)
}


#' WIP DOESNT WORK YET
#' Standard Deviations of Numerical Columns
#' @description This functions returns the Standard Deviation
#' for a numerical column in a dataset. 
#' 
#' @example stdev(dplyr::starwars$height)
stddev <- function(column) {
  # TODO Should accept any type, filter our everything except numeric
  # then return dataframe of standard deviations
  sapply(as.numeric(column), sd(na.rm = TRUE))
}