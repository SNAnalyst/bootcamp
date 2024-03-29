% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_rows.R
\name{random_rows}
\alias{random_rows}
\title{Print random rows}
\usage{
random_rows(x, nobs = 10)
}
\arguments{
\item{x}{the \code{matrix} or \code{data.frame}}

\item{nobs}{the number of rows (observations) that should be printed.}
}
\value{
nothing, output is printed
}
\description{
Print rows from a matrix or data.frame
}
\details{
Useful for inspection of a dataset. Will print randomly drawn 
rows from a \code{matrix} or \code{data.frame}.

The rows are printed in the order or appearance in \code{x}.
}
\examples{
random_rows(bootcamp::movie)
}
