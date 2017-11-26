#' Print discriptives using skimr::skim
#'
#' @description
#' Function to effortless use skimr::skim() in Windows.
#' Problem: histogram functionality of skimr::skim works only in Windows if locale is set to Chinese
#' Solution: Set locale to Chinese and back to German afterwards
#'
#' @param df Data frame to get descriptives from
#'
#' @return NULL
#'
#' @author Dominik Vogel
#'
#' @seealso skimr::skim()
#'
#' @examples
#' myskim(mtcars)
#'
#' @export
myskim <- function(df) {
  Sys.setlocale( locale='Chinese' )
  a <- skimr::skim(df)
  print(a)
  Sys.setlocale(locale = "German")
  return = NULL
}

