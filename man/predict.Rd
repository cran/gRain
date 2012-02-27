\name{predict.grain}
\alias{predict.grain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make predictions from a probabilistic network}
\description{
  Makes predictions (either as the most likely state or as the
  conditional distributions) of variables conditional on finding (evidence) on other variables
  in an independence network.
}
\usage{
\method{predict}{grain}(object, response, predictors, newdata, type = "class", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A grain object}
  \item{response}{A vector of response variables to make predictions on}
  \item{predictors}{A vector of predictor variables to make predictions
    from. Defaults to all variables that are note responses.}
  \item{newdata}{A data frame}
  \item{type}{If "class", the most probable class is returned; if
    "distribution" the conditional distrubtion is returned.}
  \item{...}{Not used}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A list with components
  \item{pred}{A list with the predictions}
  \item{pFinding}{A vector with the probability of the finding (evidence) on
    which the prediction is based}
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
\seealso{\code{\link{grain}}}

%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
