\name{gmInstance}

\alias{newgmInstance}
\alias{newgmInstance.cptspec}
\alias{newgmInstance.graphNEL}
%\alias{newgmInstance.ugsh}

%\alias{makebn}
%\alias{resetbn}
\alias{nodeNames}
\alias{nodeStates}
\alias{nodeNames.gmInstance}
\alias{nodeStates.gmInstance}


\alias{as.gmInstance}
\alias{as.gmInstance.huginNet}

\alias{plot.compgmInstance}
\alias{plot.cpt-gmInstance}
\alias{plot.dag-gmInstance}
\alias{plot.ug-gmInstance}



%% Remove this later
%\alias{as.probnet}
%\alias{as.probnet.cptspec}
%\alias{as.probnet.dagsh}
%\alias{as.probnet.huginNet}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical Independence Network}
\description{
  The 'newgmInstance' builds a graphical independence network.
}
\usage{
%makebn(cptlist, gmData, description="ProbNet", root=NULL, propagate=FALSE, trace=0)
%as.probnet(x, gmData, description="ProbNet",  control=list(),trace=0,...)
newgmInstance(x, gmData, description="ProbNet",  control=list(), trace=0,...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An argument to build an independence network from.}
  %\item{cptlist}{A list of conditional probability tables (see Examples below)}
  \item{gmData}{A gmData object (see Examples below)}
  \item{description}{A text describing the network}
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
  An object of class "gmInstance"
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ %
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{cptable}},
  %\code{\link{ctab}},
  \code{\link[gRbase]{gmData}}}
\examples{

## Asia (chest clinique) example - using a gmData object
##
chestNames <- c("asia", "smoke", "tub", "lung", "bronc", "either", "xray", "dysp")
gmd <- newgmData(chestNames,valueLabels=c("yes","no"))
summary(gmd)

p.a    <-cptable('asia', values=c(0.01,0.99),gmData=gmd)
p.t.a  <-cptable('tub',pa='asia', values=c(0.05,0.95,0.01,0.99),gmData=gmd)
p.s    <-cptable('smoke', values=c(0.5,0.5), gmData=gmd)
p.l.s  <-cptable('lung',pa='smoke', values=c(0.1,0.9,0.01,0.99), gmData=gmd)
p.b.s  <-cptable('bronc',pa='smoke', values=c(0.6,0.4,0.3,0.7), gmData=gmd)
p.e.lt <-cptable('either',pa=c('lung','tub'),values=c(1,0,1,0,1,0,0,1),gmData=gmd)
p.x.e  <-cptable('xray',pa='either', values=c(0.98,0.02,0.05,0.95), gmData=gmd)
p.d.be <-cptable('dysp',pa=c('bronc','either'), values=c(0.9,0.1,0.7,0.3,0.8,0.2,0.1,0.9), gmData=gmd)
 
cptlist <- list(p.a, p.t.a, p.s, p.l.s, p.b.s, p.e.lt, p.x.e, p.d.be)
cptlist[[1]]
cptlist[[2]]

bn <- newgmInstance(cptspec(cptlist), gmd)
bn

summary(bn)
plot(bn)


## Asia (chest clinique) example - without using a gmData object
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

plist <- cptspec(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
pn <- newgmInstance(plist)
pn


summary(pn)
plot(pn)


## Create network from gmData (with data) and graph specification.
## There are different ways:
##
data(HairEyeColor)
d   <- as.gmData(HairEyeColor)
dag <- newDAGlist(list(~Hair, ~Eye+Hair, ~Sex+Hair))
class(dag)
ug <- newUGlist(list(~Eye+Hair, ~Sex+Hair))
class(ug)

## 1) Create directly from dag
b1  <- newgmInstance(dag,d)
class(b1)

## 2) Extract cpt's for dag from gmData and build network from cpt's
x<-dag2cptspec(dag,d)
class(x)
b2 <- newgmInstance(x,d)
class(b2)

## 3) Build model from undirected (decomposable) graph
b3  <- newgmInstance(ug,d)
class(b3)

## Simple example - one clique only in triangulated graph
##
plist <- cptspec(list(p.a, p.t.a))
pn <- newgmInstance(plist)
querygm(pn)

## Simple example - disconnected network
##
plist <- cptspec(list(p.a, p.t.a, p.s))
pn <- newgmInstance(plist)
querygm(pn)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
