#' Top Authors
#'
#' @param data
#' A pubblications dataframe
#'
#' @return
#' A data frame with the leading cities in the publications
#' @export
top_city = function(data) {
  V = data[, c("City.of.Research.organization")]

  city_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  city_auths = lapply(city_auths, trimws)

  city_auths = data.frame(table(unlist(city_auths)))

  # Rename the columns
  colnames(city_auths) = c("city", "count")

  # Sort by descending count and reset row names
  city_auths = city_auths[order(city_auths$count, decreasing = TRUE),]
  rownames(city_auths) = seq_len(nrow(city_auths))

  return(city_auths)
}
