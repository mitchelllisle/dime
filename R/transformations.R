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
