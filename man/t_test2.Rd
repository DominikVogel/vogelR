\name{t_test2}
\alias{t_test2}
\title{t-test from means and sds}
\description{Function to perform a two independent sample t-test
based on sample size, means, and standard deviations}
\usage{t_test2(m1, m2, s1, s2, n1, n2, m0 = 0, equal.variance = FALSE)}
\arguments{
  \item{m1, m2}{Sample means}
  \item{s1, s2}{Sample standard deviations}
  \item{n1, n2}{Sample sizes}
  \item{m0}{the null value for the difference in means to be tested for. Default is 0.}
  \item{equal.variance}{Whether or not to assume equal variance. Default is FALSE.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{returns vector with difference of means, standard error, t-value, p-value}
\references{https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha}
\author{Dominik Vogel}
\note{
Copied from https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
t <- t_test2(3.5, 4, 0.7, 0.8, 100, 200)

## The function is currently defined as
t_test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
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
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~htest }% use one of  RShowDoc("KEYWORDS")
