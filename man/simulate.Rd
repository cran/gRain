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
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}  
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

\dontrun{

tf <- system.file("huginex", "chest_clinic.net", package = "gRain")
chestf <- loadHuginNet(tf, trace=1)
chest <- as.grain(chestf)

simulate(chest,n=10)

chest2 <- setFinding(chest, c("VisitToAsia", "Dyspnoea"),
c("yes","yes"))

simulate(chest2,n=10)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}

