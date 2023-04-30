#' Top Research Organizations
#'
#' @param data
#' A publications dataframe
#' @return
#' A dataframe with the leading research organisations in the publications
#' @export
#' @examples
#' top_org(data)
top_org = function(data){
  V=data[, c("Research Organizations standardized")]

  org_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  org_auths = lapply(org_auths, trimws)

  org_auths = data.frame(table(unlist(org_auths)))

  # Rename the columns
  colnames(org_auths) = c("org", "count")

  # Sort by descending count and reset row names
  org_auths = org_auths[order(org_auths$count, decreasing = TRUE), ]
  rownames(org_auths) = seq_len(nrow(org_auths))

  # Show the resulting dataframe
  head(org_auths,20)

  return(org_auths)
}


