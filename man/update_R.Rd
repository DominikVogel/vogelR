\name{update_R}
\alias{update_R}
\title{Update R Core using installr::updateR}
\description{Function to call installr::updateR to update R Core}
\usage{update_R()}
\arguments{NULL}
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
}
\examples{
update_R()

## The function is currently defined as
update_R <- function() {
  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()

  #* Add an error if installr is not installed
  if (!requireNamespace("installr", quietly = TRUE))
    ArgumentCheck::addError(
      msg = "package 'installr' is needed for this function. Please install it",
      argcheck = Check
    )

  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)

  installr::updateR()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~internal }% use one of  RShowDoc("KEYWORDS")
