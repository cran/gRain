\name{cptable}
\alias{cptable}
\alias{print.cpt}
\alias{cptspec}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Create conditional probability tables (CPTs)}
\description{
  Creates conditional probability tables of the form p(v|pa(v)). 
}
\usage{
cptable(v,pa=NULL, levels=NULL, values = NULL, normalize = TRUE, smooth = 0)
cptspec(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{A vertex}
  \item{pa}{The parents of vertex v}
  \item{values}{Probabilities; recycled if necessary}

  \item{normalize}{See 'details' below.}
  \item{smooth}{See 'details' below.}
  \item{levels}{See 'details' below.}
  \item{x}{A list of cpts}
}
\details{
  If \code{normalize=TRUE} then for each configuration of the parents
  the probabilities are normalized to sum to one. 
  
  If \code{smooth} is non--zero then zero entries of
  \code{values} are replaced with \code{smooth} before normalization takes place.

%  If no \code{gmData} object is given, then \code{levels} (giving the
%  levels of 'v') must be given. Otherwise  \code{levels} is ignored.
}

\value{
  If a \code{gmData} object is given, then 'cpt' returns an object of
  class 'ctab' (which is a general representation of a table).

  If no  \code{gmData} object is given, then
  'cpt' returns an object of class 'cptTemplate'. 

  'cptspec' returns an object of class 'cptspec' (which is just a list
  with a special class attribute).
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ %
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{\code{\link{ctab}}}
\examples{

yn <- c("yes","no")
ynm <- c("yes","no","maybe")
a    <- cptable(~asia, values=c(1,99),levels=yn)
t.a  <- cptable(~tub+asia, values=c(5,95,1,99,1,999),levels=ynm)
d.a  <- cptable(~dia+asia, values=c(5,5,1,99,100,999),levels=ynm)
cptspec(list(a,t.a,d.a))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
