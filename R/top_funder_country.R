#' Top Funder Country
#'
#' @param data
#' A pubblications dataframe
#'
#' @return
#' A data frame with the leading cities in the publications
#' @export
top_funder_country = function(data) {
  V = data[, c("Funder.Country")]

  group_c_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  group_c_auths = lapply(group_c_auths, trimws)

  group_c_auths = data.frame(table(unlist(group_c_auths)))

  # Rename the columns
  colnames(group_c_auths) = c("group", "count")

  # Sort by descending count and reset row names
  group_c_auths = group_c_auths[order(group_c_auths$count, decreasing = TRUE),]
  rownames(group_c_auths) = seq_len(nrow(group_c_auths))

  return(group_c_auths)
}
