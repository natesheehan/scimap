#' Top Countries
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_top_country = function(data){
  V=data[, c("Country of Research organization")]

  pub_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  pub_auths = lapply(pub_auths, trimws)

  author_counts = data.frame(table(unlist(pub_auths)))

  # Rename the columns
  colnames(author_counts) = c("authors", "count")

  # Sort by descending count and reset row names
  author_counts = author_counts[order(author_counts$count, decreasing = TRUE), ]
  rownames(author_counts) = seq_len(nrow(author_counts))

  # Show the resulting dataframe
  head(author_counts,20)
}

