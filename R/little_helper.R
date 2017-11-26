#' Print all rows of a tibble
#'
#' This function prints a tibble with all its rows
#'
#' @param tibble The tibble to be printed
#'
#' @return NULL
#'
#' @author Dominik Vogel
#'
#' @examples
#' tib <- tibble::tibble(a = 1:90,
#'                      b = 1:90,
#'                      c = 1:90,
#'                      d = 1:90,
#'                      e = 1:90,
#'                      f = 1:90,
#'                      g = 1:90,
#'                      h = 1:90,
#'                      i = 1:90,
#'                      j = 1:90,
#'                      k = 1:90,
#'                      l = 1:90,
#'                      m = 1:90,
#'                      n = 1:90,
#'                      o = 1:90,
#'                      p = 1:90,
#'                      q = 1:90,
#'                      r = 1:90,
#'                      s = 1:90,
#'                      t = 1:90,
#'                      u = 1:90,
#'                      v = 1:90,
#'                      w = 1:90)
#' tibble_print_rows(tib)
#' @export
tibble_print_rows <- function(tibble) {
  default <- getOption("tibble.print_max")
  options(tibble.print_max = Inf)
  print(tibble)
  options(tibble.print_max = default)
}


#' Print all rows and columns of a tibble
#'
#' @param tibble The tibble to be printed
#'
#' @return NULL
#'
#' @author Dominik Vogel
#'
#' @examples
#' tib <- tibble::tibble(a = 1:90,
#'                      b = 1:90,
#'                      c = 1:90,
#'                      d = 1:90,
#'                      e = 1:90,
#'                      f = 1:90,
#'                      g = 1:90,
#'                      h = 1:90,
#'                      i = 1:90,
#'                      j = 1:90,
#'                      k = 1:90,
#'                      l = 1:90,
#'                      m = 1:90,
#'                      n = 1:90,
#'                      o = 1:90,
#'                      p = 1:90,
#'                      q = 1:90,
#'                      r = 1:90,
#'                      s = 1:90,
#'                      t = 1:90,
#'                      u = 1:90,
#'                      v = 1:90,
#'                      w = 1:90)
#' tibble_print_all(tib)
#' @export
tibble_print_all <- function(tibble) {
  default_width <- getOption("tibble.width")
  default_rows <- getOption("tibble.print_max")
  options(tibble.width = Inf)
  options(tibble.print_max = Inf)
  print(tibble)
  options(tibble.width = default_width)
  options(tibble.print_max = default_rows)
}




#' Print all columns of a tibble
#'
#' @param tibble The tibble to be printed
#'
#' @return NULL
#'
#' @author Dominik Vogel
#'
#' @examples
#' tib <- tibble::tibble(a = 1:90,
#'                      b = 1:90,
#'                      c = 1:90,
#'                      d = 1:90,
#'                      e = 1:90,
#'                      f = 1:90,
#'                      g = 1:90,
#'                      h = 1:90,
#'                      i = 1:90,
#'                      j = 1:90,
#'                      k = 1:90,
#'                      l = 1:90,
#'                      m = 1:90,
#'                      n = 1:90,
#'                      o = 1:90,
#'                      p = 1:90,
#'                      q = 1:90,
#'                      r = 1:90,
#'                      s = 1:90,
#'                      t = 1:90,
#'                      u = 1:90,
#'                      v = 1:90,
#'                      w = 1:90)
#' tibble_print_vars(tib)
#' @export
tibble_print_vars <- function(tibble) {
  default <- getOption("tibble.width")
  options(tibble.width = Inf)
  print(tibble)
  options(tibble.width = default)
}
