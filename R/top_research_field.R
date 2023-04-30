#' Top Research Field
#'
#' @param data
#'
#' @return
#' A dataframe with the leading research fields in the publications
#'
#' @export
#'
#' @examples
#' top_research_field(data)
top_research_field = function(data) {
  V = data[, c("Fields of Research (ANZSRC 2020)")]

  field_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  field_auths = lapply(field_auths, trimws)

  field_auths = data.frame(table(unlist(field_auths)))

  # Rename the columns
  colnames(field_auths) = c("city", "count")

  # Sort by descending count and reset row names
  field_auths = field_auths[order(field_auths$count, decreasing = TRUE),]
  rownames(field_auths) = seq_len(nrow(field_auths))

  # Show the resulting dataframe
  head(field_auths, 20)

  return(field_auths)
}
