#' Top Funder Group
#'
#' @param data
#' A pubblications dataframe
#'
#' @return
#' A data frame with the leading cities in the publications
#' @export
top_funder_group = function(data) {
  V = data[, c("Funder.Group")]

  group_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  group_auths = lapply(group_auths, trimws)

  group_auths = data.frame(table(unlist(group_auths)))

  # Rename the columns
  colnames(group_auths) = c("group", "count")

  # Sort by descending count and reset row names
  group_auths = group_auths[order(group_auths$count, decreasing = TRUE),]
  rownames(group_auths) = seq_len(nrow(group_auths))

  return(group_auths)
}
