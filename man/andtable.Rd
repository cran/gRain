\name{andtable}
\alias{andtable}
\alias{ortable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Conditional probability tables based on logical dependcies}
\description{
  Generate conditional probability tables based on the logical
  expressions AND and OR. 
}
\usage{
andtable(v, pa1 = c(TRUE, FALSE), pa2 = c(TRUE, FALSE), levels)
ortable(v, pa1 = c(TRUE, FALSE), pa2 = c(TRUE, FALSE), levels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{Specifications of the names in P(v|pa1,...pak). See section 'details'
    for information about the form of the argument.}
  \item{pa1,pa2}{The coding of the logical parents}
  \item{levels}{The levels (or rather labels) of v, see 'examples' below}
}
\details{
  Regarding the form of the argument \code{v}: To specify \eqn{P(a|b,c)}
  one may write \code{~a|b+c} or \code{~a+b+c} or
  \code{c("a","b","c")}. Internally, the last form is used. Notice that
  the \code{+} operator is used as a separator only. The order of the
  variables is important so \code{+} does not commute.
}
\value{
  A cptable.
}
\references{
  Søren Højsgaard (2012). Graphical Independence Networks with the
  gRain Package for R. Journal of Statistical Software, 46(10), 1-26.
  \url{http://www.jstatsoft.org/v46/i10/}.
}

\author{Søren Højsgaard, sorenh at mail dot dk}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{cptable}}}
\examples{
ortable(c("v","A","B"), levels=c("yes","no"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
