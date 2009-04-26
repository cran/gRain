\name{propagate.grain}
\alias{propagate.grain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Propagate a graphical independence network (a Bayesian network)}
\description{
  Propagation refers to calibrating the cliques of the junction tree so
  that the clique potentials are consistent on their intersections
}
\usage{
propagate.grain(object, trace = object$trace,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A grain object}
  \item{trace}{For debugging info}
  \item{...}{Currently not used}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  A compiled and propagated grain object.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{grain}},\code{\link{compile}}}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{models}% __ONLY ONE__ keyword per line
