\name{predict.compgmInstance}
\alias{predict.compgmInstance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make predictions from a probabilistic network}
\description{
  Makes predictions (either as the most likely state or as the
  conditional distributions) of variables conditional on evidence on other variables
  in an independence network.
}
\usage{
\method{predict}{compgmInstance}(object, response, predictors, newdata, type = "class", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A gmInstance object}
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
  \item{pevidence}{A vector with the probability of the evidence on
    which the prediction is based}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{newgmInstance}}}

%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
