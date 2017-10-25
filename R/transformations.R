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
