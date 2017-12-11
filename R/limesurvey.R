#' Fix variable names for data frames exportet from LimeSurvey
#'
#' @description Function to get rid of the 'X[Y]' variable names in data frames exported from LimeSurvey
#'
#' @param df Data frame from LimeSurvey
#'
#' @return df
#'
#' @author Dominik Vogel
#'
#' @examples
#' lime_names(mtcars)
#'
#' @export
lime_names <- function(df) {
  df <- stats::setNames(df, gsub(".*\\[(.+)\\]", "\\1", names(df)))
  return(df)
}
