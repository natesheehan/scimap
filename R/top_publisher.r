#' Top Publisher
#'
#' @param data
#'
#' @return
#' A list of
#' @export
#'
#' @examples
plot_top_journals = function(data){
  V=data[, c("Publisher")]

  journal_counts = data.frame(table((V)))

  # Rename the columns
  colnames(journal_counts) = c("journal", "count")

  # Sort by descending count and reset row names
  journal_counts = journal_counts[order(journal_counts$count, decreasing = TRUE), ]
  rownames(journal_counts) = seq_len(nrow(journal_counts))

  # Show the resulting dataframe
  head(journal_counts,20)

  return(journal_counts)
}


