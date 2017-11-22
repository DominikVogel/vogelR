\name{extract_lm}
\alias{extract_lm}
\title{Extract parameters from stats::lm-models}
\description{Function extracts coefficients and p-values from stats::lm-models}
\usage{extract_lm(model, var, parameter)}
\arguments{
  \item{model}{The stats::lm-model (must be build in advance)}
  \item{var}{Name of Variable for with parameter should be extracted}
  \item{paramter}{"b" = coefficient, "p" = p-value}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{returns "b = ..." or "p = ..."}
\references{}
\author{Dominik Vogel}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
extract_hlm()
}
\examples{
model <- stats::lm(mpg ~ cyl + am, data = mtcars)
extract_lm(model, "cyl", "p")

## The function is currently defined as
extract_lm <- function(model, var, parameter = c("p", "b")) {
  if (!requireNamespace("broman", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it.",
         call. = FALSE)
  }
  sum_model <- summary(model)
  ifelse(parameter == "p",
         ifelse(sum_model[["coefficients"]][var,4] >= 0.001,
                out <- paste0("p = ",
                              broman::myround(sum_model[["coefficients"]][var,4],
                                      digits = 3)),
                out <- "p < 0.001"),
         ifelse(parameter == "b",
                out <- paste0("b = ",
                              broman::myround(sum_model[["coefficients"]][[var,1]],
                                      digits = 2)),
                NULL)
  )
  return(out)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~model }% use one of  RShowDoc("KEYWORDS")
