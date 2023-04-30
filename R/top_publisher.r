#' Top Publisher
#'
#' @param data
#' A publication dataframe
#' @return
#' A dataframe with the leading publishers in the publications
#' @export
top_journals = function(data){
  V=data[, c("Publisher")]

  journal_counts = data.frame(table((V)))

  # Rename the columns
  colnames(journal_counts) = c("journal", "count")

  # Sort by descending count and reset row names
  journal_counts = journal_counts[order(journal_counts$count, decreasing = TRUE), ]
  rownames(journal_counts) = seq_len(nrow(journal_counts))

  return(journal_counts)
}


