% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cptable_special.R
\name{mendel}
\alias{mendel}
\title{Mendelian segregation}
\usage{
mendel(allele, names = c("child", "father", "mother"))
}
\arguments{
\item{allele}{A character vector.}

\item{names}{Names of columns in dataframe.}
}
\description{
Generate conditional probability table for Mendelian
segregation.
}
\note{
No error checking at all on the input.
}
\examples{
## Inheritance of the alleles "y" and "g"

men <- mendel(c("y","g"), names=c("ch", "fa", "mo"))
men

}
