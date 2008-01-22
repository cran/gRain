\name{querygm}
\alias{querygm}
\alias{querygm.gmInstance}
\alias{enterEvidence}
\alias{retractEvidence}
\alias{evidence}
\alias{pevidence}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Query an independence network}
\description{
  Query an independence network, i.e. obtain the conditional
  distribution of a set of variables given evidence on other variables. 
}
\usage{
querygm(object, nodes = nodeNames(object), normalize = TRUE, type =
c("marginal", "joint", "conditional"), trace = 0)

enterEvidence(object, nodes=NULL, states=NULL, evlist=NULL, propagate=TRUE)
retractEvidence(object, nodes=NULL, propagate=TRUE)
evidence(object)
pevidence(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A "gmInstance" object}
  \item{nodes}{A vector of nodes}
  \item{states}{A vector of states (of the nodes given by 'nodes')}
  \item{evlist}{An alternative way of specifying evidence, see examples below.}
  \item{propagate}{Should the network be propagated?}
  \item{normalize}{Should the results be normalized to sum to one.}
  \item{type}{Should marginals (for each node), the joint for all nodes,
  or the conditional of the first node given the rest be returned.}
  \item{trace}{Debugging information}

}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A list of tables with potentials
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{
  \code{\link{ctab}, \link{cpt}}
}
\examples{


testfile <- system.file("huginex", "chest_clinic.net", package = "gRain")
chest <- as.gmInstance(loadHuginNet(testfile, trace=1))
print(class(chest))


qb <- querygm(chest)
qb

lapply(qb, as.numeric) # Safe
sapply(qb, as.numeric) # Risky


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{utilities}% __ONLY ONE__ keyword per line
