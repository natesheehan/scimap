#' Import Data
#'
#' @param file
#' a file
#' @param format
#' a forma
#' @return
#' A dataframe from a specified file route and database
#' @export
import_data = function(file,format){

  switch (format,
    csv = {
      data = read.csv(file, na.strings = "", skip = 1)

      # Convert all numeric columns to character
      numeric_cols = sapply(data, is.numeric)
      data[, numeric_cols] = lapply(data[, numeric_cols], as.character)

      # Replace all NA values in character columns with empty strings
      character_cols = sapply(data, is.character)
      D = data[, character_cols] = lapply(data[, character_cols], function(x) ifelse(is.na(x), "", x))

      D = as.data.frame(D)

      return(D)
    },
    excel = {
      cat("fuck microsoft")
    }
  )
}
