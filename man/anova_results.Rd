\name{anova_results}
\alias{anova_results}
\title{Results of ANOVA in APA standard}
\description{Function to print results of an ANOVA in APA standard (F(df, df) = F; p = p)}
\usage{anova_results(y, x)}
\arguments{
  \item{y}{a numeric vector (dependent variable)}
  \item{y}{a factor (grouping variable)}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{"F(df, df) = F; p = p"}
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
df <- data.frame(happy = c(rnorm(n = 10, mean = 3, sd = 0.5), rnorm(n = 10, mean = 3.4, sd = 0.5),
                           rnorm(n = 10, mean = 3, sd = 0.5)),
                 treatment = factor(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               2, 2, 2, 2, 2, 2, 2, 2, 2, 2)))
anova_results(df$happy, df$treatment)


## The function is currently defined as
anova_results <- function(y, x) {
  devtools::use_package("stats")
  devtools::use_package("broman")
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
}
