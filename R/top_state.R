#' Top Authors
#'
#' @param data
#' A publications dataframe
#' @return
#' A dataframe with the leading states in the publications
#' @export
top_state = function(data) {
  V = data[, c("State.of.Research.organization")]

  state_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  state_auths = lapply(state_auths, trimws)

  state_auths = data.frame(table(unlist(state_auths)))

  # Rename the columns
  colnames(state_auths) = c("state", "count")

  # Sort by descending count and reset row names
  state_auths = state_auths[order(state_auths$count, decreasing = TRUE),]
  rownames(state_auths) = seq_len(nrow(state_auths))

  return(state_auths)
}
