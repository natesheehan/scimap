#' Top Publication Type
#'
#' @param data
#' A publication dataframe
#' @return
#' A dataframe with the leading access types in the publications
#' @export
top_type = function(data) {
  type = data[, c("Publication.Type")]
  access = as.data.frame(table(type))
  return(access)
}
