#' Top Countries
#'
#' @param data
#' A publications dataframe
#'
#' @return
#' A dataframe with the leading countries in the publications
#' @export
top_country = function(data){
  V=data[, c("Country of Research organization")]

  country_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  country_auths = lapply(country_auths, trimws)

  country_auths = data.frame(table(unlist(country_auths)))

  # Rename the columns
  colnames(country_auths) = c("country", "count")

  # Sort by descending count and reset row names
  country_auths = country_auths[order(country_auths$count, decreasing = TRUE), ]
  rownames(country_auths) = seq_len(nrow(country_auths))

  # Show the resulting dataframe
  head(country_auths,20)

  return(country_auths)
}

