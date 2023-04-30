#' Top Authors
#'
#' @param data
#'
#' @return
#' A dataframe with the leading authors in the publications
#'
#' @export
#'
#' @examples
#' file = "Dimensions-Publication-2023-04-30_00-09-19.csv"
#' data = import_data(file,"csv")
#' top_authors(data)
top_authors = function(data){
  V=data[, c("Authors")]

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

  return(author_counts)
}


