#' Type Checking
#' @description This functions returns the types for a given dataset or
#' field.
#'
#' @param  x the data to return types for
#'
#' @usage types(x = dplyr::starwars)
#'
dm_types <- function(x) {
  type_data <- data.frame(Type = sapply(x, class))

  return(type_data)
}

#' Distribution of data based off Categories in column
#' @description This functions works off categorical data. It
#' will return the frequency and percentage of data points
#' that fit to each distinct category.
#' @param column the column to run distribution checks on
#'
#' @usage dist(column = dplyr::starwars$gender)
dm_dist <- function(column) {
  df <- data.frame(cbind(freq=table(column), percentage=prop.table(table(column))*100))

  return(df)
}

#' Distribution of data based off Categories in column
#' @description This functions works off categorical data. It
#' will return the frequency and percentage of data points
#' that fit to each distinct category.
#' @param column the column to run distribution checks on
#'
#' @usage dm_dist_column(column = dplyr::starwars$gender)
dm_dist_column <- function(column) {
  df <- data.frame(cbind(freq=table(column), percentage=prop.table(table(column))*100))

  return(df)
}

#' Distribution of data based off Categories in Dataframe
#' @description This functions works off categorical data. It
#' will return the frequency and percentage of data points
#' that fit to each distinct category.
#' @param data the data to run distribution checks on
#'
#' @usage dm_dist_df(data = dplyr::starwars)
dm_dist_df <- function(data) {
  df <- lapply(data, dime::dm_dist_column)

  return(df)
}



#' Standard Deviations of Numerical Columns
#' @description This functions returns the Standard Deviation
#' for a numerical column in a dataset.
#'
#' @param dataset the dataset to run stdev checks on
#'
#' @usage stdev(dataset = dplyr::starwars)
dm_stddev <- function(dataset) {
  df <- dataset[sapply(dataset, function(x) is.integer(x) || is.numeric(x) || is.double(x))]
  df <- mlr::mlr_replace_all_na(df)

  stddev_result <- data.frame(StandardDeviation = sapply(df, sd))

  return(stddev_result)
}

#' Check the 'Skewness' of a data set
#' @description This function returns the skewness of some data. The larger the skew to the
#' left, the stronger the negative skew. The larges the skew to the right, the stronger the
#' positive skew.
#'
#' @param dataset the dataset to run skewness checks on
#'
#' @usage dm_skewness(dataset = dplyr::starwars)
dm_skewness <- function(dataset) {
  df <- dataset[sapply(dataset, function(x) is.integer(x) || is.numeric(x) || is.double(x))]
  df <- mlr::mlr_replace_all_na(df)

  skewness <- data.frame(apply(df, 2, e1071::skewness))

  return(skewness)
}


#' Check the Correlation of numerical data
#' @description This function returns the correlation of numerical values in a dataset.
#' Deviations from zero show more positive or negative correlation. Values above approximately 0.75 or below -0.75
#' are perhaps more interesting as they show a high correlation or high negative correlation.
#'
#' Values of 1 and -1 show full positive or negative correlation.
#'
#' @param dataset the dataset to run skewness checks on
#'
#' @usage dm_cors(dataset = dplyr::starwars)
dm_cors <- function(dataset) {
  df <- dataset[sapply(dataset, function(x) is.integer(x) || is.numeric(x) || is.double(x))]
  df <- mlr::mlr_replace_all_na(df)

  correlations <- cor(df)

  return(correlations)
}
