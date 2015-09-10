\name{setFinding}

\alias{setFinding}
\alias{retractFinding}
\alias{getFinding}
\alias{pFinding}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Set, retrieve, and retract finding in Bayesian network.
}

\description{
  Set, retrieve, and retract finding in Bayesian network.
  NOTICE: The functions described here are kept only for backward
  compatibility; please use the corresponding evidence-functions in the
  future.
}

\usage{
setFinding(object, nodes=NULL, states=NULL, flist=NULL, propagate=TRUE)
getFinding(object)
pFinding(object)
retractFinding(object, nodes=NULL, propagate=TRUE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A "grain" object}
  \item{nodes}{A vector of nodes}
  \item{states}{A vector of states (of the nodes given by 'nodes')}
  \item{flist}{An alternative way of specifying findings, see
    examples below.}
  \item{propagate}{Should the network be propagated?}
}
\note{
  NOTICE: The functions described here are kept only for backward compatibility;
  please use the corresponding evidence-functions in the future:

  \code{setEvidence()} is an improvement of  \code{setFinding()} (and as
  such \code{setFinding} is obsolete). Users are recommended to use
  \code{setEvidence()} in the future.

  \code{setEvidence()} allows to specification of "hard evidence"
  (specific values for variables) and likelihood evidence (also known as
  virtual evidence) for variables.

  The syntax of \code{setEvidence()} may change in the future.
}

%% \value{
%%   A list of tables with potentials.
%% }

\references{ Søren Højsgaard (2012). Graphical Independence Networks
  with the gRain Package for R. Journal of Statistical Software, 46(10),
  1-26.  \url{http://www.jstatsoft.org/v46/i10/}.  }

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
\seealso{
  \code{\link{setEvidence}}
  \code{\link{getEvidence}}
  \code{\link{retractEvidence}}
  \code{\link{pEvidence}}
  \code{\link{querygrain}}  
}
\examples{

## setFindings
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
chest <- grain(plist)

## These two forms are equivalent
bn1 <- setFinding(chest, nodes=c("asia","xray"), states=c("yes", "yes"))
bn2 <- setFinding(chest, flist=list(c("asia","yes"), c("xray", "yes")))

getFinding(bn1)
getFinding(bn2)

pFinding(bn1)
pFinding(bn2)

bn1 <- retractFinding(bn1, nodes="asia")
bn2 <- retractFinding(bn2, nodes="asia")

getFinding(bn1)
getFinding(bn2)

pFinding(bn1)
pFinding(bn2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{utilities}% __ONLY ONE__ keyword per line
