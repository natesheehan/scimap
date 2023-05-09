#' report
#'
#' @param data
#' a publication dataframe
#' @param n
#' the number of limit the report to
#' @return
#' a terminal report based on the publication data
#' @export
library(pander)

report = function(data,n){


  timespan = paste0(min(dummy_data$PubYear),":",max(dummy_data$PubYear)) # timespan e.g. 1993:2020
  sources = nrow(as.data.frame(unique(dummy_data$Source.title)))# number of sources (journals, books etc)
  np = nrow(dummy_data) # number of publications
  mc = mean(as.numeric(dummy_data$Times.cited)) # mean citations
  access = top_access(data)
  type = top_type(data)
  results = list(
    authors = top_authors(data)[1:n,],
    city = top_city(data)[1:n,],
    country = top_country(data)[1:n,],
    funder = top_funder(data)[1:n,],
    funder_country = top_funder_country(data)[1:n,],
    funder_group = top_funder_group(data)[1:n,],
    journal = top_journals(data)[1:n,],
    keywords = top_keywords(data)[1:n,],
    orgs = top_org(data)[1:n,],
    publisher = top_journals(data)[1:n,],
    research_fields = top_research_field(data)[1:n,],
    states = top_state(data)[1:n,]
  )

  cat("Your Results:\n")
  print(type)

  # print results using pander
  for (i in names(results)) {
    if (is.data.frame(results[[i]])) {
      pander::pander(results[[i]], style = "grid", caption = i)
    } else {
      print(results[[i]])
    }
  }

  cat("\n\nAccess Table:\n\n")
  pander::pander(table(access), style = "grid", caption = "Access Table")

}
