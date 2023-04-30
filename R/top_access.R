#' Top Access
#'
#' @param data
#' A publication dataframe
#' @return
#' A dataframe with the leading access types in the publications
#' @export
#' @examples
#' Add your data to top_access
top_access = function(data) {
  V = data[, c("Open.Access")]
  V = trimws(gsub("All OA;", "", V, fixed = TRUE))
  access = as.data.frame(table(V))
  table(V)
}
