\name{propagate.grain}
\alias{propagate.grain}
\alias{propagateLS}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Propagate a graphical independence network (a Bayesian network)}
\description{
  Propagation refers to calibrating the cliques of the junction tree so
  that the clique potentials are consistent on their intersections
}
\usage{
\method{propagate}{grain}(object, details = object$details,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A grain object}
  \item{details}{For debugging info}
  \item{...}{Currently not used}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  A compiled and propagated grain object.
}
\references{
  Søren Højsgaard (2012). Graphical Independence Networks with the
  gRain Package for R. Journal of Statistical Software, 46(10), 1-26.
  \url{http://www.jstatsoft.org/v46/i10/}.
}

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
%\note{ ~~further notes~~
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{grain}},\code{\link[gRbase]{compile}}}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{models}% __ONLY ONE__ keyword per line
