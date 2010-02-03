\name{compileCPT,compilePOT}
\alias{compileCPT}
\alias{compilePOT}
\alias{print.CPTspec}
\alias{print.POTspec}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compile conditional probability tables / cliques potentials.}
\description{
  Compile conditional probability tables / cliques potentials as a
  preprocessing step for creating a graphical independence network
}
\usage{
compileCPT(x)
compilePOT(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{To \code{compileCPT} x is a list of conditional probability
    tables; to \code{compilePOT}, x is a list of clique potentials}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  \code{compileCPT} returns a list of class 'cptspec'
  \code{compilePOT} returns a list of class 'potspec'
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
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
