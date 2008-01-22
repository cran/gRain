\name{ctab}

\alias{ctab}
\alias{varNames.ctab}
\alias{nLevels.ctab}
\alias{valueLabels.ctab}
\alias{ctab}
\alias{ctabop}
\alias{ctabdiv}
\alias{ctabmult}
\alias{ctabmarg}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Representation of and operations on multidimensional tables}
\description{General representation of multidimensional 
  tables (by \code{ctab} objects).
  Operations on these include multiplication, division and
  marginalization.
}


\usage{
ctab(names, levels, values = 1, normalize = c("none", "first", "all"), smooth = 0)
ctabop(t1, t2, op="*")
ctabdiv(t1, t2)
ctabmult(t1, t2)
ctabmarg(t1, marg, normalize=FALSE)

\method{varNames}{ctab}(x)
\method{nLevels}{ctab}(x)
\method{valueLabels}{ctab}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{names}{Names of variables defining table}
  \item{levels}{Levels of variables}
  \item{values}{Table values}
  \item{t1,t2,x}{Objects of class "ctab"}
  \item{op}{Operation to perform. Either "*" or "/" (where 0/0=0)}
  \item{marg}{Set of nodes to marginalize onto}
  \item{normalize}{Should result be normalized, see 'Details' below.}
  \item{smooth}{Should values be smoothed, see 'Details' below.}
}
\details{
  A ctab object represents a table defined by a set of variables and
  their levels, together with the values of the table. E.g. f(a,b,c) can
  be a table with a,b,c representing levels of binary variable
  
  If \code{normalize="first"} then for each configuration of the parents,
  "pa", the probabilities are normalized to sum to one. Thus f(a,b,c)
  becomes a conditional probability table of the form p(a|b,c). If
  \code{normalize="all"} then the sum over all entries of f(a,b,c) is
  one. 
  
  If \code{smooth} is non--zero then zero entries of
  \code{values} are replaced with \code{smooth} before normalization takes place.

}
\value{
  All ctab functions return an object of class \code{ctab}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}

\seealso{\code{\link{cpt}}}

\examples{
t1 <- ctab(c("gender","answer"),list(c('male','female'),c('yes','no')), values=1:4)
t2 <- ctab(c("answer","category"), list(c('yes','no'),2), values=1:4+10)
t3 <- ctab(c("category","foo"), c(2,2), values=1:4+100)

varNames(t1)
nLevels(t1)
valueLabels(t1)

ctabmult(t1,t2)
ctabdiv(t1,t2)

tgender <- ctabmarg(t1, "gender")
t1
tgender
ctabdiv(t1,tgender)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
