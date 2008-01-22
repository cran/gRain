\name{cpt}
\alias{cpt}
\alias{print.cpt}
\alias{cptspec}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Create conditional probability tables (CPTs)}
\description{
  Creates conditional probability tables of the form p(v|pa(v)). 
}
\usage{
cpt(v, pa = NULL, values = NULL, gmData=NULL, normalize = TRUE, smooth = 0, levels=NULL)
cptspec(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{Variable}
  \item{pa}{Possible parents}
  \item{gmData}{Name of gmData object (where to look for v and pa)}
  \item{values}{Probabilities; recycled if necessary}

  \item{normalize}{See 'details' below.}
  \item{smooth}{See 'details' below.}
  \item{levels}{See 'details' below.}
  \item{x}{A list of cpts}
}
\details{
  If \code{normalize=TRUE} then for each configuration of the parents,
  "pa", the probabilities are normalized to sum to one. 
  
  If \code{smooth} is non--zero then zero entries of
  \code{values} are replaced with \code{smooth} before normalization takes place.

  If no \code{gmData} object is given, then \code{levels} (giving the
  levels of 'v') must be given. Otherwise  \code{levels} is ignored.
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
\seealso{\code{\link{ctab}}}
\examples{
g <- newgmData(letters[1:5],nLevels=2)

t1 <- cpt("a",pa="b", gmData=g,values=1:4)

t2 <- cpt("a",pa=c("b","c"), levels=c("y","n"),values=1:4)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
