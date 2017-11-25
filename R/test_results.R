# Chi2
chi_results <- function(x, y) {
  devtools::use_package("stats")
  devtools::use_package("broman")
  suppressWarnings(chi_result <- chisq.test(table(x, y)))
  p <- ifelse(chi_result[["p.value"]] >= 0.001,
              paste0("p = ", broman::myround(chi_result[["p.value"]], 2)),
              "p < 0.001")
  result <- paste0("Chi2(",
         as.integer(chi_result[["parameter"]][["df"]]), ") = ",
         broman::myround(chi_result[["statistic"]][["X-squared"]],2), "; ",
         p)
  return(result)
}



