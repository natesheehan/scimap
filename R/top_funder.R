#' Top Funder
#'
#' @param data
#' A publications dataframe
#'
#' @return
#' A dataframe with the leading funders in the publications
#' @export
top_funder = function(data){
  V=data[, c("Funder")]

  funder_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  funder_auths = lapply(funder_auths, trimws)

  funder_auths = data.frame(table(unlist(funder_auths)))

  # Rename the columns
  colnames(funder_auths) = c("authors", "count")

  # Sort by descending count and reset row names
  funder_auths = funder_auths[order(funder_auths$count, decreasing = TRUE), ]
  rownames(funder_auths) = seq_len(nrow(funder_auths))

  # Show the resulting dataframe
  head(funder_auths,20)

  return(funder_auths)
}


