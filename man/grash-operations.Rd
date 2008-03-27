\name{grash-operations}


\alias{moralize}
\alias{mcs}
\alias{triangulate}
\alias{ripOrder}
\alias{jTree}

\alias{edges-methods}
\alias{nodes-methods}
\alias{edges}
\alias{nodes}

\alias{edges,ugsh,ANY-method}
\alias{edges,dagsh,ANY-method}
\alias{nodes,graphsh-method}

\alias{edges,compgmInstance}
\alias{edges,compgmInstance,ANY-method}
\alias{edges,cpt-gmInstance}
\alias{edges,cpt-gmInstance,ANY-method}
\alias{nodes,compgmInstance-method}
\alias{nodes,cpt-gmInstance-method}

\alias{show}
\alias{show,ugsh-method}
\alias{show,dagsh-method}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Operations on undirected and directed acyclic graphs.}
\description{
  Make operations on undirected and directed acyclic graphs.
}

\usage{
moralize(dag)
mcs(ug,amat=as.adjmat(ug), vn=colnames(amat), root=NULL, index=FALSE)
triangulate(ug,method="standard", nLevels, matrix=FALSE)
ripOrder(ug,root=NULL,nLevels=NULL)
jTree(ug, method="standard", vn=nodes(ug),nLevels, control=list())


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dag}{A directed graph (of class "dagsh")}
  
  \item{ug}{An undirected graph (of class "ugsh")}
  \item{method}{The triangulation method, \code{"standard"} is a C
    implementation of a minimum clique weight heuristic,
    \code{"mcwh"} is a Fortran implementation of the same (kindly
    provided by Peter Green) and \code{"R"} is a corresponding R
    implementation. }
  \item{matrix}{Should the triangulated graph be returned as an
    adjacency matrix}
  \item{amat}{The graph represented as an adjancecy matrix}
  \item{vn}{The node names as they appear in the adjancecy matrix}
  \item{root}{Variables which must be in the first clique in the rip-ordering}
  \item{nLevels}{Number of levels of the nodes in the graph}
  \item{index}{If TRUE, then a permutation is returned}
  \item{control}{Currently not used}
  \item{\dots}{Currently not used.}
}
\details{
  The jTree (for "junction tree") is just a wrapper for a call of
  \code{triangulate} followed by a call of \code{ripOrder}.
}
\value{
  \code{moralize} and   \code{triangulate} returns objects of class
  "graphsh".

  \code{mcs} returns a vector or NULL.

  \code{ripOrder} returns a list with cliques, separators etc.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{
  \code{\link{newugsh}}, \code{\link{newug}},
  \code{\link{newdagsh}}, \code{\link{newdag}}
}
\examples{

## Undirected graphs
##
ug <- newug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
edges(ug)
nodes(ug)
mcs(ug)
ripOrder(ug)

## Directed graphs
##
dag <- newdag(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
edges(dag)
nodes(dag)
moralize(dag)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

