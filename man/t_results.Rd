\name{t_results}
\alias{t_results}
\title{Results of t-test in APA standard}
\description{Function to print results of a t-test in APA standard (t(df) = t; p = p)}
\usage{t_results(y, x)}
\arguments{
  \item{y}{a numeric vector (dependent variable)}
  \item{x}{a numeric vector or a factor (grouping variable)}
  \item(alternative}{a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.}
  \item{mu}{a number indicating the true value of the mean (or difference in means if you are performing a two sample test). Default is 0.}
  \item{paired}{a logical indicating whether you want a paired t-test. Default is FALSE}
  \item{var.equal}{a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used. Default is FALSE}
  \item{conf.level}{confidence level of the interval. Default is 0.95}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{"t(df) = t; p = p"}
\references{}
\author{Dominik Vogel}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
set.seed(1)
df <- data.frame(happy = c(rnorm(n = 10, mean = 3, sd = 0.5), rnorm(n = 10, mean = 4, sd = 0.5)),
                 treatment = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

t_results(y = df$happy, x = df$treatment)

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

