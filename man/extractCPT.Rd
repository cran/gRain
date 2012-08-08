\name{extractCPT,extractPOT}
\alias{extractCPT}
\alias{extractCPT.table}
\alias{extractCPT.data.frame}
\alias{extractPOT}
\alias{extractPOT.table}
\alias{extractPOT.data.frame}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract conditional probabilities and clique potentials from data}
\description{
  Extract conditional probabilities and clique potentials from data
}
\usage{
extractCPT(x, graph, V = nodes(graph), smooth = 0)
extractPOT(x, graph, smooth = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A contingency table (an array)}
  \item{graph}{To extractCPT graph is a DAG while to extractPOT graph is
  an undirected triangulated graph.}
  \item{V}{The set of vertices for which CPTs should be extracted}
  \item{smooth}{See 'details' below}
}
\details{
  If \code{smooth} is non--zero then zero entries of
  \code{values} are replaced with \code{smooth} before normalization takes place.
}
\value{
  extractCPT: A list of conditional probability tables
  extractPOT: A list of clique potentials
}
\references{
  Søren Højsgaard (2012). Graphical Independence Networks with the
  gRain Package for R. Journal of Statistical Software, 46(10), 1-26.
  \url{http://www.jstatsoft.org/v46/i10/}.
}

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{compileCPT}}, \code{\link{compilePOT}}, \code{\link{grain}}}
%\examples{
%% To be written
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
