\name{myskim}
\alias{myskim}
\title{Print discriptives using skimr::skim}
\description{Function to effortless use skimr::skim() in Windows.
  Problem: histogram functionality of skimr::skim works only in Windows if locale is set to Chinese
  Solution: Set locale to Chinese and back to German afterwards}
\usage{myskim(df)}
\arguments{
  \item{df}{Data frame to get descriptives from}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{NULL}
\references{}
\author{Dominik Vogel}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
skimr::skim()
}
\examples{
myskim(mtcars)

## The function is currently defined as
myskim <- function(df) {
  devtools::use_package("rlang")
  devtools::use_package("pillar")
  devtools::use_package("skimr")
  Sys.setlocale( locale='Chinese' )
  a <- skimr::skim(df)
  print(a)
  Sys.setlocale(locale = "German")
  return = NULL
}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~univar }% use one of  RShowDoc("KEYWORDS")
