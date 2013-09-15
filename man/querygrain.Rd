\name{querygrain}
\alias{querygrain}
\alias{querygrain.grain}



\alias{setFinding}
\alias{retractFinding}
\alias{getFinding}
\alias{pFinding}

\alias{setEvidence}
\alias{retractEvidence}
\alias{getEvidence}
\alias{pEvidence}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set evidence and query a network}
\description{
  Query an independence network, i.e. obtain the conditional
  distribution of a set of variables given finding (evidence) on other variables.
}
\usage{
querygrain(object, nodes = nodeNames(object), normalize = TRUE,
           type = "marginal", result="array", details = 0)

setEvidence(object, nodes=NULL, states=NULL, nslist=NULL, propagate=TRUE)
retractEvidence(object, nodes=NULL, propagate=TRUE)
getEvidence(object)
pEvidence(object)

setFinding(object, nodes=NULL, states=NULL, flist=NULL, propagate=TRUE)
retractFinding(object, nodes=NULL, propagate=TRUE)
getFinding(object)
pFinding(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A "grain" object}
  \item{nodes}{A vector of nodes}
  \item{states}{A vector of states (of the nodes given by 'nodes')}
  \item{flist}{An alternative way of specifying findings (evidence), see
    examples below.}
  \item{nslist}{An alternative way of specifying findings (evidence), see examples below.}
  \item{propagate}{Should the network be propagated?}
  \item{normalize}{Should the results be normalized to sum to one.}
  \item{type}{Valid choices are \code{"marginal"} which gives the
    marginal distribution for each node in \code{nodes}; \code{"joint"}
    which gives the joint distribution for \code{nodes} and
    \code{"conditional"} which gives the conditional distribution for
    the first variable in \code{nodes} given the other variables in
    \code{nodes}. }
  \item{result}{If "data.frame" the result is returned as a data frame
    (or possibly as a list of dataframes).}
  \item{details}{Debugging information}

}
\note{
  \code{setEvidence()} is an improvement of  \code{setFinding()} (and as
  such \code{setFinding} is obsolete). Users are recommended to use
  \code{setEvidence()} in the future.

  \code{setEvidence()} allows to specification of "hard evidence"
  (specific values for variables) and likelihood evidence (also known as
  virtual evidence) for variables.

  The syntax of \code{setEvidence()} may change in the future.
}

\value{
  A list of tables with potentials.
}

\references{ Søren Højsgaard (2012). Graphical Independence Networks
  with the gRain Package for R. Journal of Statistical Software, 46(10),
  1-26.  \url{http://www.jstatsoft.org/v46/i10/}.  }

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{
  \code{\link{cptable}}
}
\examples{
testfile <- system.file("huginex", "chest_clinic.net", package = "gRain")
chest <- loadHuginNet(testfile, details=0)
qb <- querygrain(chest)
qb

lapply(qb, as.numeric) % Safe
sapply(qb, as.numeric) % Risky

## setFinding / setEvidence

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


## 1) These two forms are identical
setEvidence(chest, c("asia","xray"), c("yes", "yes"))
setFinding(chest, c("asia","xray"), c("yes", "yes"))

## 2) Suppose we do not know with certainty whether a patient has
## recently been to Asia. We can then introduce a new variable
## "guess.asia" with "asia" as its only parent. Suppose
## p(guess.asia=yes|asia=yes)=.8 and p(guess.asia=yes|asia=no)=.1
## If the patient is e.g. unusually tanned we may set
## guess.asia=yes and propagate. This corresponds to modifying the
## model by the likelihood (0.8, 0.1) as
setEvidence(chest, c("asia","xray"), list(c(0.8,0.1), "yes"))

## 3) Hence, the same result as in 1) can be obtained with
setEvidence(chest, c("asia","xray"), list(c(1, 0), "yes"))

## 4) An alternative specification using nslist (node-state-list) is
setEvidence(chest, nslist=list("asia"=c(1, 0), "xray"="yes"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{utilities}% __ONLY ONE__ keyword per line
