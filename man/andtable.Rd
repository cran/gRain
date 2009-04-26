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
andtable(v, pa = NULL, pa1 = c(TRUE, FALSE), pa2 = c(TRUE, FALSE), levels)
ortable(v, pa = NULL, pa1 = c(TRUE, FALSE), pa2 = c(TRUE, FALSE), levels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{A vertex}
  \item{pa}{The parents of vertex v}
  \item{pa1,pa2}{The coding of the logical parents}
  \item{levels}{The levels (or rather labels) of v, see 'examples' below}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  A cptable.
  }
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
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
