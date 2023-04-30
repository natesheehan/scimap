#' Top Access
#'
#' @param data
#'
#' @return
#' A dataframe with the leading access types in the publications
#'
#' @export
#'
#' @examples
#' top_access(data)
top_access = function(data) {
  V = data[, c("Open Access")]
  V$`Open Access` = trimws(gsub("All OA;", "", V$`Open Access`, fixed = TRUE))
  table(V)
  access = as.data.frame(table(V))
}
