# Get dummy data from Dimensions API
dummy_data = import_data("data-raw/Dimensions-Publications.csv","csv")
dummy_data = dummy_data[sample(nrow(dummy_data), 2, replace = FALSE), ]
dummy_data = data.frame(lapply(dummy_data, function(x) iconv(x, "UTF-8", "ASCII", sub="")))

usethis::use_data(dummy_data, internal = TRUE, overwrite = TRUE)

