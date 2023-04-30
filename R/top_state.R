#' Top Authors
#'
#' @param data
#'
#' @return
#' A dataframe with the leading states in the publications
#' @export
#'
#' @examples
#' top_State(data)
#'
top_state = function(data) {
  V = data[, c("City of Research organization")]

  state_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  state_auths = lapply(state_auths, trimws)

  state_auths = data.frame(table(unlist(state_auths)))

  # Rename the columns
  colnames(state_auths) = c("state", "count")

  # Sort by descending count and reset row names
  state_auths = state_auths[order(state_auths$count, decreasing = TRUE),]
  rownames(state_auths) = seq_len(nrow(state_auths))

  # Show the resulting dataframe
  head(state_auths, 20)

  return(state_auths)
}
