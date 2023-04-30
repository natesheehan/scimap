#' Top Acess
#'
#' @param file
#' @param format
#'
#' @return
#' A dataframe with the leading access types in the publications
#'
#' @export
#'
#' @examples
#' file = "Dimensions-Publication-2023-04-30_00-09-19.csv"
#' data = import_data(file,"csv")
#' top_access(data)
top_access = function(data) {
  V = data[, c("Open Access")]
  V$`Open Access` = trimws(gsub("All OA;", "", DATA$`Open Access`, fixed = TRUE))
  table(V)
  access = as.data.frame(table(V))
}
