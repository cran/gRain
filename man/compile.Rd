\name{compile,propagate}
\alias{compile}
\alias{propagate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compile and propagate functions}
\description{
  \code{compile} and \code{propagate} are generic functions which invoke
  particular methods which depend on the class of the first argument
}
\usage{
compile(object, ...)
propagate(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object to be compiled or propagated}
  \item{\dots}{Additional arguments which depends on the class of the object}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  The value returned depends on the class of the first argument.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{compile.grain}}, \code{\link{propagate.grain}}}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
