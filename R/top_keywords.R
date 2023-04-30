#' Top Keywords
#'
#' @param data
#' A publications dataframe
#'
#' @return
#' A dataframe with the leading keywords in the publications
#' @export
top_keywords = function(data){
  V=data[, c("MeSH.terms")]

  keywords_auths = sapply(V, function(x)
    strsplit(as.character(x), split = ";"))

  keywords_auths = lapply(keywords_auths, trimws)

  keywords_auths = data.frame(table(unlist(keywords_auths)))

  # Rename the columns
  colnames(keywords_auths) = c("authors", "count")

  # Sort by descending count and reset row names
  keywords_auths = keywords_auths[order(keywords_auths$count, decreasing = TRUE), ]
  rownames(keywords_auths) = seq_len(nrow(keywords_auths))

  return(keywords_auths)
}


