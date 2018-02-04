#' List 2 Test
#' @description Takes a column that is made up of a list and converts to
#' concatenated values with a separator specified. Defaults to comma.
#'
#' @param column the column to transform
#' @param sep the separator to use
#'
dm_lst2txt <- function (column, sep = ", ")
{
  loadNamespace("stringr")
  ret <- sapply(column, function(x) {
    ret <- stringr::str_c(x, collapse = sep)
    if (identical(ret, character(0))) {
      NA
    }
    else {
      ret
    }
  })
  as.character(ret)
}

#' String Clean
#' @description Cleans up text in column by removing characters like `\n, \t` etc.
#'
#' dm_strclean(word)
dm_strclean <- function(words) {
  words <- stringr::str_replace_all(words, "\n|\t", " ")
  words <- stringr::str_replace_all(words, " +", " ")
  words <- stringr::str_replace_all(words, "\\.\\.+", ".")
  words <- stringr::str_trim(words)
}

#' Census Reshape
#'  @description The census data from ABS typically comes in a wide format. This function will
#' convert it into a long format as it's often easier to work with and fit for more
#' purposes.
#'
#' @param
census_reshape <- function(data, id){
  molten <- melt(data, id = id, na.rm = FALSE)
  result <- data.frame(molten)

  return(result)
}

#' Null to NA
#'
#' @description I have often come up against issue of 'differing rows' when trying
#' to bind dataframes together. To counter this it was always a pain. This function
#' help with this process by converting all NULLs found by missing values and converting
#' them to either a string 'N/A' or a logical NA.
#'
#' @param x the dataframe or list to convert
#' @param asString whether or not to return the NA as a string or logical.
#' False = Logical, True = String
nullToNAString <- function(x, asString = FALSE) {
  if(asString == FALSE){
    NAtoApply <- NA
  } else {
    NAtoApply <- 'N/A'
  }

  x[sapply(x, is.null)] <- NAtoApply
  return(x)
}

#' Replace all NAs in a column or dataframe
#'
#' @description This function is used to replace all NAs within
#' a column or dataframe. It is useful for when you want to do numerical
#' arithmetic on a field but are running into issues with NA values
#'
replace_all_na <- function (dataframe)
{
  if (!is.data.frame(dataframe)) {
    message("Please supply a dataframe")
  }
  else {
    dataframe[is.na(dataframe)] <- 0
    data.frame(dataframe)
  }
}

#' Split Data for Training and Testing
#'
#' @description  This is often a manual step in any tutorial I've used which
#' I found strange. This is a simple function to split a data set
#' into a list contain an object of test and train.
#'
#' @param data the data to split into train and test objects
#' @param splitPercentage The percent of data to split for training.
#' Default is .7 or 70%
dm_testTainSplit <- function(data, splitPercentage = .7){
  dt <- sort(sample(nrow(data), nrow(data) * splitPercentage))
  train <- data[dt,]
  test <- data[-dt,]

  output <- list(train = train, test = test)

  return(output)
}
