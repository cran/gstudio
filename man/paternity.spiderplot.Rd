\name{paternity.spiderplot}
\alias{paternity.spiderplot}
\title{Creates a graphical depiction of paternity}
\usage{
  paternity.spiderplot(pop = NULL, paternity = NULL,
    indID = "IndID", X = "lon", Y = "lat", ...)
}
\arguments{
  \item{pop}{A population that has all the individuals.}

  \item{paternity}{A 'paternity' object from
  \code{paternity}}

  \item{indID}{The column header for adults}

  \item{X}{Heading for x coordinates}

  \item{Y}{Heading for y coordinates}

  \item{...}{Other options to pass to the plot function}
}
\value{
  Nothing
}
\description{
  Simple plot of a paternity analysis results.
}
\note{
  This function creates a plot of the inferred paternity
  cases
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

