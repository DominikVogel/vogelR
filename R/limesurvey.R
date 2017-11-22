lime_names <- function(df) {
  devtools::use_package("stats")
  df <- stats::setNames(gsub(".*\\[(.+)\\]", "\\1", names(df)), df)
  return(df)
}
