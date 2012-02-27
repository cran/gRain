\name{loadHuginNet}
\alias{loadHuginNet}
\alias{saveHuginNet}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Load and save Hugin net files}
\description{
  These functions can load a net file saved in the 'Hugin format' into R
  and save a network in R as a file in the 'Hugin format'.
}

\usage{
loadHuginNet(file, description, details = 0)
saveHuginNet(gin, file, details = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gin}{An independence network}
  \item{file}{Name of HUGIN net file. Convenient to give the file the
    extension '.net'}
  \item{description}{A text describing the network, defaults to \code{file}}
  \item{details}{Debugging information}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  An object (a list) of class "huginNet".
}

\references{
  Søren Højsgaard (2012). Graphical Independence Networks with the
  gRain Package for R. Journal of Statistical Software, 46(10), 1-26.
  \url{http://www.jstatsoft.org/v46/i10/}.
}

\author{Søren Højsgaard, sorenh at mail dot dk}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}

\seealso{\code{\link{grain}} }
\examples{

%% Load HUGIN net file
tf <- system.file("huginex", "chest_clinic.net", package = "gRain")
chest <- loadHuginNet(tf, details=1)
chest 

%% Save a copy
td <- tempdir()
saveHuginNet(chest, paste(td,"/chest.net",sep=''))

%% Load the copy
chest2 <- loadHuginNet(paste(td,"/chest.net",sep=''))

tf <- system.file("huginex", "golf.net", package = "gRain")
golf <- loadHuginNet(tf, details=1)

saveHuginNet(golf, paste(td,"/golf.net",sep=''))
golf2 <- loadHuginNet(paste(td,"/golf.net",sep=''))


}
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
