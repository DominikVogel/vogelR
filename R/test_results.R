chi_results <- function(x, y) {
  devtools::use_package("stats")
  devtools::use_package("broman")
  suppressWarnings(chi_result <- chisq.test(table(x, y)))
  result <- paste0("Chi2(",
         as.integer(chi_result[["parameter"]][["df"]]),
         ") = ",
         broman::myround(chi_result[["statistic"]][["X-squared"]], 2),
         "; p = ",
         broman::myround(chi_result[["p.value"]], 2))
  return(result)
}



