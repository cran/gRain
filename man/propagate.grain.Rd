\name{propagate.grain}
\alias{propagate.grain}
\alias{propagateLS}

\alias{propagate__}
\alias{propagateLS__}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Propagate a graphical independence network (a Bayesian network)}
\description{
  Propagation refers to calibrating the cliques of the junction tree so
  that the clique potentials are consistent on their intersections
}
\usage{
\method{propagate}{grain}(object, details = object$details, ...)
propagate__(object, details = object$details, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A grain object}
  \item{details}{For debugging info}
  \item{...}{Currently not used}
}
\details{
	The \code{propagate} method invokes \code{propagateLS} which is a pure
	R implementation of the Lauritzen-Spiegelhalter algorithm.

	The function \code{propagate__} invokes \code{propagateLS__} which is
	a c++ implementation of the Lauritzen-Spiegelhalter algorithm.

	The c++ based version is several times faster than the purely R based
	version, and after some additional testing the c++ based version will
	become the default.
}
\value{
  A compiled and propagated grain object.
}
\references{
  Søren Højsgaard (2012). Graphical Independence Networks with the
  gRain Package for R. Journal of Statistical Software, 46(10), 1-26.
  \url{http://www.jstatsoft.org/v46/i10/}.
}

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
%\note{
%
%}

\seealso{
	\code{\link{grain}},
	\code{\link[gRbase]{compile}}
}
\examples{

yn   <- c("yes","no")
a    <- cptable(~asia,              values=c(1,99), levels=yn)
t.a  <- cptable(~tub+asia,          values=c(5,95,1,99), levels=yn)
s    <- cptable(~smoke,             values=c(5,5), levels=yn)
l.s  <- cptable(~lung+smoke,        values=c(1,9,1,99), levels=yn)
b.s  <- cptable(~bronc+smoke,       values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either+lung+tub,   values=c(1,0,1,0,1,0,0,1), levels=yn)
x.e  <- cptable(~xray+either,       values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp+bronc+either, values=c(9,1,7,3,8,2,1,9), levels=yn)
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
pn    <- grain(plist)
pnc  <- compile(pn, propagate=FALSE)

if (require(microbenchmark))
    microbenchmark(
        propagate(pnc),
        propagate__(pnc) )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{models}% __ONLY ONE__ keyword per line
