#' Add mean index to data frame
#'
#' @description Add a mean index of a defined group of variables to a data frame
#'
#' @param df Data frame to add index to
#' @param name Name of the new index
#' @param vars Variables to combine to an index
#'
#' @return df
#'
#' @author Dominik Vogel
#'
#' @examples
#' df <- mtcars
#' mean_index2(df, "mean", vars = c(c("gear", "carb")))
#' @export
mean_index2 <- function(df, name, vars) {
  M1 <- dplyr::select(df, vars) # Generate matrix with necessary variables only
  M2 <- rowMeans(M1, na.rm = TRUE) # rowMeans for Matrix M
  M2 <- tibble::tibble(M2)
  colnames(M2) <- name
  df <- dplyr::bind_cols(df, M2)
  return(df)
}



