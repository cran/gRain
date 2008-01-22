\name{graphsh}
 
\alias{newugsh}
\alias{newug}
\alias{newdagsh}
\alias{newdag}

\alias{plot.compgmInstance}
\alias{plot.cpt-gmInstance}
\alias{plot.dag-gmInstance}
\alias{plot.ug-gmInstance}

\alias{plot.ugsh}
\alias{plot.dagsh}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create undirected and directed graphs}
\description{
  Functions for creating undirected and directed acyclic graphs from an
  adjacency list.
}
\usage{
%newugsh(x, ..., short = FALSE)
%newdagsh(x, ..., short = FALSE)

newug(..., short=FALSE)
newdag(..., short=FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  %\item{x}{ ~~Describe \code{x} here~~ }
  \item{\dots}{An incidence list (a list of edges)}
  \item{short}{If TRUE then the edges/vertices are not printed. Can be
    convenient for large graphs.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  An object of class "ugsh" or "dagsh".
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}

\seealso{\code{\link{queryg}}}
\examples{
ug <- newug(c("me","ve"),c("me","al"),c("ve","al"),c("al","an"),c("al","st"),c("an","st"))
ug <- newug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)

dag <- newdag(c("me","ve"),c("me","al"),c("ve","al"),c("al","an"),c("al","st"),c("an","st"))
dag <- newdag(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)

edges(ug)
nodes(ug)

edges(dag)
nodes(dag)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
