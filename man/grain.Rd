\name{grain}

\alias{grain}
%\alias{print.grain}
\alias{grain.CPTspec}
\alias{grain.POTspec}
\alias{grain.graphNEL}
\alias{grain.dModel}

\alias{nodeNames}
\alias{nodeStates}
\alias{nodeNames.grain}
\alias{nodeStates.grain}

% \alias{as.grain}
% \alias{as.grain.huginNet}

\alias{plot.grain}
\alias{iplot.grain}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical Independence Network}
\description{
  The 'grain' builds a graphical independence network.
}
\usage{
grain(x, data, control=list(), smooth=0, details=0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An argument to build an independence network from. Typically
    a list of conditional probability tables, a DAG or an undirected
    graph. In the two latter cases, data must also be provided.}
  \item{data}{An optional data set (currently must be an array/table)}
  \item{control}{A list defining controls, see 'details' below.}
  \item{smooth}{A (usually small) number to add to the counts of a table
    if the grain is built from a graph plus a dataset.}
  \item{details}{Debugging information.}
  \item{...}{Additional arguments, currently not used.}
}
\details{
  If 'smooth' is  non-zero then entries of 'values' which a zero are
  replaced by the value of 'smooth' - BEFORE any normalization takes place.
}
\value{
  An object of class "grain"
}
\references{
  Søren Højsgaard (2012). Graphical Independence Networks with the
  gRain Package for R. Journal of Statistical Software, 46(10), 1-26.
  \url{http://www.jstatsoft.org/v46/i10/}.
}

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
% \note{
%   There are two methods for displaying networks: 'plot' and 'iplot'.

%   1) The 'plot' method uses the Rgraphviz package and this package requires
%   that the Graphviz program is installed. Installation of Rgraphviz and
%   Graphviz is not made automatically when gRain is installed.

%   2) The 'iplot' method uses the igraph package which is installed
%   automatically when gRain is installed.
% }

\seealso{
  \code{\link{cptable}},
  \code{\link{compile.grain}},
  \code{\link{propagate.grain}},
  \code{\link{setFinding}},
  \code{\link{setEvidence}},
  \code{\link{getFinding}},
  \code{\link{pFinding}},
  \code{\link{retractFinding}}
  %\code{\link[gRbase]{gmData}}
}
\examples{

## Asia (chest clinic) example:
yn   <- c("yes","no")
a    <- cptable(~asia,              values=c(1,99), levels=yn)
t.a  <- cptable(~tub+asia,          values=c(5,95,1,99), levels=yn)
s    <- cptable(~smoke,             values=c(5,5), levels=yn)
l.s  <- cptable(~lung+smoke,        values=c(1,9,1,99), levels=yn)
b.s  <- cptable(~bronc+smoke,       values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either+lung+tub,   values=c(1,0,1,0,1,0,0,1), levels=yn)
x.e  <- cptable(~xray+either,       values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp+bronc+either, values=c(9,1,7,3,8,2,1,9), levels=yn)
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
pn    <- grain(plist)
pn
summary(pn)
plot(pn)
pnc <- compile(pn, propagate=TRUE)

## If we want to query the joint distribution of the disease nodes,
## computations can be speeded up by forcing these nodes to be in
## the same clique of the junction tree:

pnc2 <- compile(pn, root=c("lung", "bronc", "tub"), propagate=TRUE)

system.time({
  for (i in 1:200)
    querygrain(pnc, nodes=c("lung","bronc", "tub"), type="joint")})
system.time({
  for (i in 1:200)
    querygrain(pnc2, nodes=c("lung","bronc", "tub"), type="joint")})


## Create network from gmData (with data) and graph specification.
## There are different ways:
data(HairEyeColor)
d   <- HairEyeColor
daG <- dagList(list(~Hair, ~Eye:Hair, ~Sex:Hair))
class( daG )
uG <- ugList(list(~Eye:Hair, ~Sex:Hair))
class( uG )

## Create directly from dag:
b1  <- grain( daG, d )
class( b1 )

## Build model from undirected (decomposable) graph
b3  <- grain( uG, d )
class( b3 )

## Simple example - one clique only in triangulated graph:
plist <- compileCPT( list(a, t.a) )
pn <- grain( plist )
querygrain(pn)

## Simple example - disconnected network:
plist <- compileCPT( list(a, t.a, s) )
pn <- grain( plist )
querygrain( pn )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
