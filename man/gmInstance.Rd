\name{gmInstance}

\alias{newgmInstance}
\alias{newgmInstance.cptspec}
\alias{newgmInstance.dagsh}
\alias{newgmInstance.ugsh}

%\alias{makebn}
%\alias{resetbn}
\alias{nodeNames}
\alias{nodeStates}
\alias{nodeNames.gmInstance}
\alias{nodeStates.gmInstance}


\alias{as.gmInstance}
\alias{as.gmInstance.huginNet}

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
\seealso{\code{\link{cpt}}, \code{\link{ctab}}, \code{\link[gRbase]{gmData}}}
\examples{

## Asia (chest clinique) example - using a gmData object
##
chestNames <- c("asia", "smoke", "tub", "lung", "bronc", "either", "xray", "dysp")
gmd <- newgmData(chestNames,valueLabels=c("yes","no"))
summary(gmd)

p.a    <-cpt('asia', values=c(0.01,0.99),gmData=gmd)
p.t.a  <-cpt('tub',pa='asia', values=c(0.05,0.95,0.01,0.99),gmData=gmd)
p.s    <-cpt('smoke', values=c(0.5,0.5), gmData=gmd)
p.l.s  <-cpt('lung',pa='smoke', values=c(0.1,0.9,0.01,0.99), gmData=gmd)
p.b.s  <-cpt('bronc',pa='smoke', values=c(0.6,0.4,0.3,0.7), gmData=gmd)
p.e.lt <-cpt('either',pa=c('lung','tub'),values=c(1,0,1,0,1,0,0,1),gmData=gmd)
p.x.e  <-cpt('xray',pa='either', values=c(0.98,0.02,0.05,0.95), gmData=gmd)
p.d.be <-cpt('dysp',pa=c('bronc','either'), values=c(0.9,0.1,0.7,0.3,0.8,0.2,0.1,0.9), gmData=gmd)
 
cptlist <- list(p.a, p.t.a, p.s, p.l.s, p.b.s, p.e.lt, p.x.e, p.d.be)
cptlist[[1]]
cptlist[[2]]

bn <- newgmInstance(cptspec(cptlist), gmd)
bn

summary(bn)
plot(bn)






## Create network from gmData and graph specification. There are two ways:
##
data(HairEyeColor)
d   <- as.gmData(HairEyeColor)
dag <- newdagsh(list(~Hair, ~Eye+Hair, ~Sex+Hair))
class(dag)

## 1) Create directly from dag
#b1  <- as.probnet(dag,d)
#class(b1)

## 2) Extract cpt's for dag from gmData and build network from cpt's
#x<-dag2cptspec(dag,d)
#class(x)
#b2 <- as.probnet(x,d)
#class(b2)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
