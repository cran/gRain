\name{simulate.grain}
\alias{simulate.grain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate from an independence network}
\description{
  Simulate data from an independence network.
}
\usage{
\method{simulate}{grain}(object, nsim = 1, seed = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An inpendence network}
  \item{nsim}{Number of cases to simulate}
  \item{seed}{An optional integer controlling the random number generatation}
  \item{\dots}{Not used...}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A data frame
}
\references{
  Søren Højsgaard (2012). Graphical Independence Networks with the
  gRain Package for R. Journal of Statistical Software, 46(10), 1-26.
  \url{http://www.jstatsoft.org/v46/i10/}.
}

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}  
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

\dontrun{

tf <- system.file("huginex", "chest_clinic.net", package = "gRain")
chest <- loadHuginNet(tf, details=1)

simulate(chest,n=10)

chest2 <- setFinding(chest, c("VisitToAsia", "Dyspnoea"),
c("yes","yes"))

simulate(chest2,n=10)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}

