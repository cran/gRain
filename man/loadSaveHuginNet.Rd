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
loadHuginNet(file, description, trace = 0)
saveHuginNet(gin, file, trace = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gin}{An independence network}
  \item{file}{Name of HUGIN net file. Convenient to give the file the
    extension '.net'}
  \item{description}{A text describing the network, defaults to \code{file}}
  \item{trace}{Debugging information}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  An object (a list) of class "huginNet".
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}

\seealso{\code{\link{grain}} }
\examples{

%% Load HUGIN net file
tf <- system.file("huginex", "chest_clinic.net", package = "gRain")
chest <- loadHuginNet(tf, trace=1)
chest 

%% Save a copy
td <- tempdir()
saveHuginNet(chest, paste(td,"/chest.net",sep=''))

%% Load the copy
chest2 <- loadHuginNet(paste(td,"/chest.net",sep=''))

tf <- system.file("huginex", "golf.net", package = "gRain")
golf <- loadHuginNet(tf, trace=1)

saveHuginNet(golf, paste(td,"/golf.net",sep=''))
golf2 <- loadHuginNet(paste(td,"/golf.net",sep=''))


}
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
