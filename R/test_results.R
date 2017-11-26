#' Results of Chi2-test in APA standard
#'
#' @description Function to print results of a Chi2-test in APA standard (Chi2(df) = chi2; p = p)
#'
#' @param x a numeric vector or a factor
#' @param y a numeric vector or a factor
#'
#' @return "Chi2(df) = chi2; p = p"
#'
#' @author Dominik Vogel
#'
#'
#' @examples
#' df <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'                   treatment = factor(c("treated", "treated", "not-treated", "treated",
#'                                        "treated", "treated", "not-treated",
#'                                        "treated", "not-treated", "treated")),
#'                    improvement = factor(c("improved","improved","not-improved","improved",
#'                                           "improved", "improved", "not-improved",
#'                                           "not-improved", "not-improved", "improved")))
#' chi_results(df$treatment, df$improvement)
#'
#' @export
chi_results <- function(x, y) {
  suppressWarnings(chi_result <- stats::chisq.test(table(x, y)))
  p <- ifelse(chi_result[["p.value"]] >= 0.001,
              paste0("p = ", broman::myround(chi_result[["p.value"]], 2)),
              "p < 0.001")
  result <- paste0("Chi2(",
         as.integer(chi_result[["parameter"]][["df"]]), ") = ",
         broman::myround(chi_result[["statistic"]][["X-squared"]],2), "; ",
         p)
  return(result)
}





#' Results of t-test in APA standard
#'
#' Function to print results of a t-test in APA standard (t(df) = t; p = p)
#'
#' @param x a numeric vector or a factor (grouping variable)
#' @param y a numeric vector (dependent variable)
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference in means if you are performing a two sample test). Default is 0.
#' @param conf.level Confidence level of the interval. Default is 0.95.
#' @param paired a logical indicating whether you want a paired t-test. Default is FALSE.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used. Default is FALSE.
#'
#' @return t(df) = t; p = p
#'
#' @author Dominik Vogel
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(happy = c(rnorm(n = 10, mean = 3, sd = 0.5),
#' rnorm(n = 10, mean = 4, sd = 0.5)),
#'                  treatment = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'                                1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
#'
#' t_results(y = df$happy, x = df$treatment)
#' @export
t_results <- function(x, y, alternative = "two.sided",
                      mu = 0, conf.level = 0.95,
                      paired = FALSE, var.equal = FALSE) {
  t_result <- stats::t.test(y ~ x,
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





#' Results of ANOVA in APA standard
#'
#' @description Function to print results of an ANOVA in APA standard (F(df, df) = F; p = p)
#'
#' @param x a factor (grouping variable)
#' @param y a numeric vector (dependent variable)
#'
#' @return "F(df, df) = F; p = p"
#'
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(happy = c(rnorm(n = 10, mean = 3, sd = 0.5),
#'                            rnorm(n = 10, mean = 3.4, sd = 0.5),
#'                            rnorm(n = 10, mean = 3, sd = 0.5)),
#'                  treatment = factor(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'                                       1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'                                       2, 2, 2, 2, 2, 2, 2, 2, 2, 2)))
#'
#' anova_results(df$happy, df$treatment)
#'
#' @author Dominik Vogel
#'
#' @export
anova_results <- function(y, x) {
  anova <- stats::aov(y ~ x)
  anova_result <- summary(anova)
  p <- ifelse(anova_result[[1]][["Pr(>F)"]][1] >= 0.001,
              paste0("p = ",
                     broman::myround(anova_result[[1]][["Pr(>F)"]][1], 2)),
              "p < 0.001")
  result <- paste0("F(",
                   as.integer(anova_result[[1]][["Df"]][1]),
                   ",",
                   as.integer(anova_result[[1]][["Df"]][2]),
                   ") = ",
                   broman::myround(anova_result[[1]][["F value"]][1], 2),
                   "; ", p)
  return(result)
}
