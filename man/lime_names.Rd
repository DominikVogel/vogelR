\name{lime_names}
\alias{lime_names}
\title{Fix variable names for data frames exportet from LimeSurvey}
\description{Function to get rid of the 'X[Y]' variable names in data frames exported from LimeSurvey}
\usage{lime_names(df)}
\arguments{
  \item{df}{Data frame}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{df}
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
lime_names(mtcars)

## The function is currently defined as
lime_names <- function(df) {
  devtools::use_package("stats")
  df <- stats::setNames(gsub(".*\\[(.+)\\]", "\\1", names(df)), df)
  return(df)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~data }% use one of  RShowDoc("KEYWORDS")
