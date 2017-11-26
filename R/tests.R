#' t-test from means and sds
#'
#' @description Function to perform a two independent sample t-test
#' based on sample size, means, and standard deviations
#'
#' @param m1 Sample mean group 1
#' @param m2 Sample mean group 2
#' @param s1 Sample S.D. group 1
#' @param s2 Sample S.D. group 2
#' @param n1 Sample size group 1
#' @param n2 Sample size group 1
#' @param m0 the null value for the difference in means to be tested for. Default is 0.
#' @param equal.variance Whether or not to assume equal variance. Default is FALSE.
#'
#' @return returns vector with difference of means, standard error, t-value, p-value
#'
#' @author Dominik Vogel
#'
#' @references https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
#'
#' @note Copied from https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
#'
#' @examples
#' t <- t_test2(3.5, 4, 0.7, 0.8, 100, 200)
#'
#' @export
t_test2 <- function(m1, m2, s1, s2, n1, n2, m0 = 0, equal.variance = FALSE)
{
  if( equal.variance==FALSE )
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) )
    df <- n1+n2-2
  }
  t <- (m1-m2-m0)/se
  dat <- c(m1-m2, se, t, 2*stats::pt(-abs(t),df))
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat)
}
