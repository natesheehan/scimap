#' Top Authors
#'
#' @param data
#'
#' @return
#' A data frame with the leading cities in the publications
#' @export
#'
#' @examples
#' file = "Dimensions-Publication-2023-04-30_00-09-19.csv"
#' data = import_data(file,"csv")
#' top_city(data)
#'
top_city = function(data) {
  V = data[, c("City of Research organization")]

  city_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  city_auths = lapply(city_auths, trimws)

  city_auths = data.frame(table(unlist(city_auths)))

  # Rename the columns
  colnames(city_auths) = c("city", "count")

  # Sort by descending count and reset row names
  city_auths = city_auths[order(city_auths$count, decreasing = TRUE),]
  rownames(city_auths) = seq_len(nrow(city_auths))

  # Show the resulting dataframe
  head(city_auths, 20)

  return(city_auths)
}
