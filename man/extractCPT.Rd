\name{extractCPT,extractPOT}
\alias{extractCPT}
\alias{extractCPT.table}
\alias{extractPOT}
\alias{extractPOT.table}

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
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\code{\link{compileCPT}}, \code{\link{compilePOT}}, \code{\link{grain}}}
\examples{
%% To be written
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
