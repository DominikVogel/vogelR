\name{extract_hlm}
\alias{extract_hlm}
\title{Extract parameters from nlme::lme-models}
\description{Function extracts coefficients and p-values from nlme::lme-models}
\usage{extract_hlm(model, var, parameter)}
\arguments{
  \item{model}{The nlme::lme-model (must be build in advance)}
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
extract_lm()
}
\examples{
model <- nlme::lme(mpg ~ cyl, random = ~1|am, data = mtcars)
extract_hlm(model, "cyl", "b")

## The function is currently defined as
extract_hlm <- function(model, var, parameter = c("p", "b")) {
  if (!requireNamespace("broman", quietly = TRUE)) {
      stop("Pkg needed for this function to work. Please install it.",
        call. = FALSE)
    }
  sum_model <- summary(model)
  ifelse(parameter == "p",
         ifelse(sum_model[["tTable"]][var, 5] >= 0.001,
                out <- paste0("p = ",
                              broman::myround(sum_model[["tTable"]][var, 5],
                                      digits = 3)),
                out <- "p < 0.001"),
         ifelse(parameter == "b",
                out <- paste0("b = ",
                              broman::myround(sum_model[["coefficients"]][["fixed"]][var],
                                      digits = 2)),
                NULL)
  )
  return(out)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~model }% use one of  RShowDoc("KEYWORDS")
