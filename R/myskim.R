myskim <- function(df) {
  devtools::use_package("rlang")
  devtools::use_package("pillar")
  devtools::use_package("skimr")
  Sys.setlocale( locale='Chinese' )
  a <- skimr::skim(df)
  print(a)
  Sys.setlocale(locale = "German")
  return = NULL
}

