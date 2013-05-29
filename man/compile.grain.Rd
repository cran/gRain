\name{compile.grain}
\alias{compile.grain}
\alias{compile.CPTgrain}
\alias{compile.POTgrain}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compile a graphical independence network (a Bayesian network)}
\description{
  Compiles a Bayesian network. This means creating a junction tree and
  establishing clique potentials.
}
\usage{
\method{compile}{grain}(object, propagate = FALSE, root = NULL, control = object$control,
details = 0, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A grain object.}
%  \item{method}{Triangulation method. Currently only "mcwh" (minimum
%    clique weight heuristic) is available.}
  \item{propagate}{If TRUE the network is also propagated meaning that
    the cliques of the junction tree are calibrated to each other.}
  \item{root}{A set of variables which must be in the root of the
    junction tree}
%  \item{smooth}{If non-zero this value is added to all potentials (to
%    avoid zeros in the joint distribution)}
  \item{control}{Controlling the compilation process. }
  \item{details}{For debugging info. Do not use.}
  \item{\dots}{Currently not used.}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  A compiled Bayesian network; an object of class \code{grain}.
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
\seealso{
  \code{\link{grain}},
  \code{\link[gRbase]{propagate}},
  \code{\link[gRbase]{triangulate}},
  \code{\link[gRbase]{rip}},
  \code{\link[gRbase]{junctionTree}}  
}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{models}% __ONLY ONE__ keyword per line
