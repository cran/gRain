\name{queryg}
\alias{queryg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Query a graph}
\description{
  \code{queryg} is a general function for querying a graph object,
  specifically graphs as created with \code{newug} and \code{newdag}.  
}
\usage{
queryg(object, type, set = NULL, set2 = NULL, set3 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A graph object; i.e. either an unidrected graph (ugsh)
    or a directed acyclic graph (dagsh)}
  \item{type}{Query type, see 'details' below.}
  \item{set,set2,set3}{Possible arguments to a graph query of type \code{type}}
}
\details{
  The \code{type} can be:
  \itemize{
    \item {adj:} Nodes adjacant to \code{set}
    \item {an:} Ancestors of \code{set}
    \item {ancestralGraph:} Ancestral graph induced by \code{set}
    \item {ancestralSet:} Ancestral set of \code{set}
    \item {cl:} Closure of \code{set}
    \item {ch:} Children of \code{set}
    \item {cliques,maxClique:} The cliques
    \item {connectedComp,concomp:} The connected components
    \item {edges:} Edges of graph
    \item {ne:} Neighbours of \code{set}
    \item {nodes:} Nodes of graph
    \item {is.complete:}
    \item {is.simplicial:}
    \item {is.triangulated:}
    \item {pa:} Parents of \code{set}
    \item {separates:} Is \code{set} and \code{set2} separated by \code{set3}
    \item {simplicialNodes:} The simplicial nodes of graph
    \item {subgraph:} Subgraph induced by \code{set}
  }
}

\value{
  Depending on the \code{type}, the output will be either a new graph or
  a vector or a list.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{
  \code{\link{newug}},
  \code{\link{newdag}}
}
\examples{
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
