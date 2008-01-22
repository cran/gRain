\name{graphsh-operations}

\alias{mcs}
\alias{triangulate}
\alias{moralize}
\alias{ripOrder}

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
triangulate(ug,root=NULL)
mcs(ug,root=NULL)
moralize(dag)
ripOrder(ug,root=NULL,nLevels=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ug}{An undirected graph (of class "ugsh")}
  \item{dag}{A directed graph (of class "dagsh")}
  \item{root}{Variables which must be in the first clique in the rip-ordering}
  \item{nLevels}{Number of levels of the nodes in the graph}
  \item{\dots}{Currently not used.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  Objects of class "graphsh".
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

