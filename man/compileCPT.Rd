\name{compileCPT,compilePOT}
\alias{compileCPT}
\alias{summary.CPTspec}
\alias{compilePOT}

% \alias{print.CPTspec}
% \alias{print.POTspec}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compile conditional probability tables / cliques potentials.}
\description{
  Compile conditional probability tables / cliques potentials as a
  preprocessing step for creating a graphical independence network
}
\usage{
compileCPT(x, forceCheck=TRUE, details=0)
compilePOT(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{To \code{compileCPT} x is a list of conditional probability
    tables; to \code{compilePOT}, x is a list of clique potentials}
  \item{forceCheck}{Controls if consistency checks of the probability
    tables should be made.}
  \item{details}{Controls amount of print out. Mainly for debugging purposes}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  \code{compileCPT} returns a list of class 'cptspec'
  \code{compilePOT} returns a list of class 'potspec'
}
\references{
  Søren Højsgaard (2012). Graphical Independence Networks with the
  gRain Package for R. Journal of Statistical Software, 46(10), 1-26.
  \url{http://www.jstatsoft.org/v46/i10/}.
}

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{extractCPT}}, \code{\link{extractPOT}}}
%\examples{
%% To be written
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
