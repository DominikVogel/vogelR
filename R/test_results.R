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




# t-test
t_results <- function(x, y, alternative = "two.sided",
                      mu = 0, conf.level = 0.95,
                      paired = FALSE, var.equal = FALSE) {
  devtools::use_package("stats")
  devtools::use_package("broman")
  t_result <- t.test(y ~ x,
                     alternative = alternative,
                     mu = mu, paired = paired,
                     conf.level = conf.level,
                     var.equal = var.equal)
  p <- ifelse(t_result[["p.value"]] >= 0.001,
              paste0("p = ", broman::myround(t_result[["p.value"]], 2)),
              "p < 0.001")
  result <- paste0("t(",
                   broman::myround(t_result[["parameter"]][["df"]], 2),
                   ") = ",
                   broman::myround(t_result[["statistic"]][["t"]], 2), "; ",
                   p)
  return(result)
}


set.seed(1)
df <- data.frame(happy = c(rnorm(n = 10, mean = 3, sd = 0.5), rnorm(n = 10, mean = 4, sd = 0.5)),
                 treatment = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

t_results(y = df$happy, x = df$treatment)
