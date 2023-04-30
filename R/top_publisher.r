#' Top Publisher
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_top_journals = function(data){
  V=data[, c("Publisher")]

  journal_counts = data.frame(table((V)))

  # Rename the columns
  colnames(journal_counts) = c("authors", "count")

  # Sort by descending count and reset row names
  journal_counts = journal_counts[order(journal_counts$count, decreasing = TRUE), ]
  rownames(journal_counts) = seq_len(nrow(journal_counts))

  # Show the resulting dataframe
  head(journal_counts,20)
}


