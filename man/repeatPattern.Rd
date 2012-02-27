\name{repeatPattern}
\alias{repeatPattern}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create repeated patterns in Bayesian networks
}
\description{
  Repeated patterns is a useful model specification short cut for
  Bayesian networks
}
\usage{
repeatPattern(plist, instances, unlist = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plist}{
    A list of conditional probability tables. The variable names must
    have the form \code{name[i]} and the \code{i} will be substituted by
    the values given in \code{instances} below. 
}
  \item{instances}{
    A vector of distinct integers
}
  \item{unlist}{
    If \code{FALSE} the result is a list in which each element is a copy
    of \code{plist} in which \code{name[i]} are substituted. If
    \code{TRUE} the result is the result of applying \code{unlist()}. 
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{
  Søren Højsgaard (2012). Graphical Independence Networks with the
  gRain Package for R. Journal of Statistical Software, 46(10), 1-26.
  \url{http://www.jstatsoft.org/v46/i10/}.
}

\author{
  Søren Højsgaard, sorenh@mail.dk
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{grain}},
  \code{\link{compileCPT}}
}
\examples{
## Specify hidden markov models. The x[i]'s are unobserved, the
## y[i]'s can be observed.

yn <- c("yes","no")

## Specify p(x0)
x.0 <- cptable(~x0, values=c(1,1), levels=yn)

## Specify transition density
x.x <- cptable(~x[i]|x[i-1], values=c(1,99,2,98),levels=yn)

## Specify emissiob density
y.x <- cptable(~y[i]|x[i],   values=c(1,99,2,98),levels=yn)

## The pattern to be repeated
pp <- list(x.x, y.x)

## Repeat pattern and create network
ppp <- repeatPattern(pp, instances=1:10)
qqq <- compileCPT(c(list(x.0),ppp))
rrr <- grain(qqq)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utils}
