\name{update.CPTgrain}
\alias{update.CPTgrain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Update a Bayesian network
}
\description{
  Update a Bayesian network
}
\usage{
\method{update}{CPTgrain}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A Bayesian network of class \code{CPTgrain}}
  \item{\dots}{
    If \code{CPTlist} is a name in the dotted list, then the object will
    be update with this value (which is assumed to be a list of
    conditional probabilities).
%%     ~~Describe \code{\dots} here~~
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
  A new Bayesian network.
  %%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  Søren Højsgaard (2012). Graphical Independence Networks with the
  gRain Package for R. Journal of Statistical Software, 46(10), 1-26.
  \url{http://www.jstatsoft.org/v46/i10/}.
}

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
\note{
  There is NO checking that the input matches the settings in the
  Bayesian network.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{

## Network for Bernulli experiment; two nodes: X and thetaX
yn  <- c("yes", "no")    # Values for X
thX.val <- c(.3, .5, .7) # Values for thetaX
prX.val <- rep(1, length(thX.val)) # Probabilities for thetaX values

thX <- cptable(~thetaX, values=prX.val, levels=thX.val)
X   <- cptable(~X|thetaX, values=rbind(thX.val,1-thX.val), levels=yn)


cptlist <- compileCPT( list(thX, X) )
bn  <- compile( grain( cptlist ) )
querygrain( setEvidence(bn, nodes="X", states="yes") )

## To insert a new prior distribution we may do as follows
## (where we can omit the process of recompiling the network)
prX.val2 <- c(.2,.3,.5)
thX2 <- cptable(~thetaX, values=prX.val2, levels=thX.val)
bn2 <- update(bn, CPTlist=compileCPT( list(thX2, X)))
querygrain( setEvidence(bn2, nodes="X", states="yes") )


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
