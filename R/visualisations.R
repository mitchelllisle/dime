#' Bar Chart
#'
#'
dm_barChart <- function(data, xAxis, yAxis, title = NULL, subtitle = NULL){
  if(is.null(subtitle)){
    subtitle <- paste0("Total Observations: ", nrow(data))
  }

  if(is.null(title)){
    title <- paste("Distribution of Values in ", xAxis)
  }
  highchart() %>%
    hc_title(text = title, align = "left") %>%
    hc_subtitle(text = subtitle, align = "left") %>%
    hc_xAxis(categories = data[,xAxis]) %>%
    hc_add_series(data[, yAxis], type = "bar")
}
