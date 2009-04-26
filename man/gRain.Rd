\name{grain}

\alias{grain}
\alias{grain.cptspec}
\alias{grain.potspec}
\alias{grain.graphNEL}

\alias{nodeNames}
\alias{nodeStates}
\alias{nodeNames.grain}
\alias{nodeStates.grain}

% \alias{as.grain}
% \alias{as.grain.huginNet}

\alias{plot.grain}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical Independence Network}
\description{
  The 'grain' builds a graphical independence network.
}
\usage{
grain(x, data, control=list(), trace=0, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An argument to build an independence network from.}
  %\item{cptlist}{A list of conditional probability tables (see Examples below)}
  \item{data}{An optional data set (currently must be an array/table)}
  %\item{description}{A text describing the network}
  %\item{root}{Nodes which are required to be in the same clique (to make
  %  computations of joint distributions faster)}
  %\item{propagate}{Should the network be propagated (i.e. initialized)}
  %\item{smooth}{Should values be smoothed, see 'Details' below.}
  \item{control}{A list defining controls, see 'details' below.}
  \item{trace}{Debugging information.}
  \item{...}{Additional arguments, currently not used.}
}
\details{
  If 'smooth' is  non-zero then entries of 'values' which a zero are
  replaced by the value of 'smooth' - BEFORE any normalization takes place. 
}
\value{
  An object of class "grain"
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ %
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{
  \code{\link{cptable}},
  \code{\link{setFinding}},
  \code{\link{getFinding}},
  \code{\link{pFinding}},
  \code{\link{retractFinding}},
  \code{\link[gRbase]{gmData}}}
\examples{


## Asia (chest clinique) example 
##
yn <- c("yes","no")
a    <- cptable(~asia, values=c(1,99),levels=yn)
t.a  <- cptable(~tub+asia, values=c(5,95,1,99),levels=yn)
s    <- cptable(~smoke, values=c(5,5), levels=yn)
l.s  <- cptable(~lung+smoke, values=c(1,9,1,99), levels=yn)
b.s  <- cptable(~bronc+smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either+lung+tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e  <- cptable(~xray+either, values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp+bronc+either, values=c(9,1,7,3,8,2,1,9), levels=yn)


plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
pn <- grain(plist)
pn

summary(pn)
plot(pn)


## Create network from gmData (with data) and graph specification.
## There are different ways:
##
data(HairEyeColor)
d   <- HairEyeColor
dag <- dagList(list(~Hair, ~Eye+Hair, ~Sex+Hair))
class(dag)
ug <- ugList(list(~Eye+Hair, ~Sex+Hair))
class(ug)

## Create directly from dag
##
b1  <- grain(dag,d)
class(b1)

## 3) Build model from undirected (decomposable) graph
b3  <- grain(ug,d)
class(b3)

## Simple example - one clique only in triangulated graph
##
plist <- compileCPT(list(a, t.a))
pn <- grain(plist)
querygrain(pn)

## Simple example - disconnected network
##
plist <- compileCPT(list(a, t.a, s))
pn <- grain(plist)
querygrain(pn)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
