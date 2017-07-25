#' Types for type checking
#' @description This functions returns the types for a given dataset or 
#' field. 
#' 
#' @param x the data to return types for
#' 
#' @usage types(dplyr::starwars)
#' 
types <- function(x) {
  type_data <- sapply(x, class)
  
  return(data.frame(type_data))
}

#' Distribution of data based off Categories in column
#' @description This functions works off categorical data. It
#' will return the frequency and percentage of data points
#' that fit to each distinct category. 
#' 
#' @usage dist(dplyr::starwars$gender)
dist <- function(column) {
  cbind(freq=table(column), percentage=prop.table(table(column))*100)
}


#' WIP DOESNT WORK YET
#' Standard Deviations of Numerical Columns
#' @description This functions returns the Standard Deviation
#' for a numerical column in a dataset. 
#' 
#' @usage stdev(dplyr::starwars)
stddev <- function(dataset) {
  df <- dataset[sapply(dataset, function(x) is.integer(x) || is.numeric(x) || is.double(x))]
  df <- mlr_replace_all_na(df)
  
  sapply(df, sd)
}