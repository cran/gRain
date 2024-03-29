% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grain_compile.R
\name{get_superset_list}
\alias{get_superset_list}
\title{Get superset for each element in a list}
\usage{
get_superset_list(x_set, y_set, warn = FALSE)
}
\arguments{
\item{x_set}{Vector or list of vectors.}

\item{y_set}{Vector or list of vectors.}

\item{warn}{Should a warning be made if an element is found.}
}
\description{
For each element (vector) x in x_set, find the first element (vector) y
in y_set such that x is contained in y
}
\examples{
x_set <- list(c("a", "b"), "e", c("b", "a"))
y_set <- list(c("f","u", "e"), c("a", "b", "c", "a"), c("b", "c", "a"))
get_superset_list(x_set, y_set)
get_superset_list(letters[1:4], y_set)
get_superset_list(letters[1:4], letters[1:10])
get_superset_list(x_set, letters[1:10])
x_set <- list(c("a", "b"), "e", c("b", "a"), "o")
y_set <- list(c("f","u", "e"), c("a", "b", "c", "a"), c("b", "c", "a"))
get_superset_list(x_set, y_set, warn=TRUE)
get_superset_list(x_set, y_set, warn=FALSE)

}
