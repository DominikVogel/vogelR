\name{chi_results}
\alias{chi_results}
\title{Results of Chi2-test in APA standard}
\description{Function to print results of a Chi2-test in APA standard (Chi2(df) = chi2; p = p)}
\usage{chi_results(x, y)}
\arguments{
  \item{x}{a numeric vector or a factor}
  \item{y}{a numeric vector or a factor}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{"Chi2(df) = chi2; p = p"}
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
df <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
               treatment = factor(c("treated", "treated", "not-treated", "treated",
                                    "treated", "treated", "not-treated",
                                    "treated", "not-treated", "treated")),
               improvement = factor(c("improved","improved","not-improved","improved",
                                      "improved", "improved", "not-improved",
                                      "not-improved", "not-improved", "improved")))

chi_results(df$treatment, df$improvement)

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~data }% use one of  RShowDoc("KEYWORDS")
