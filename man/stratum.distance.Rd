\name{stratum.distance}
\alias{stratum.distance}
\title{Calucluate pair-wise distance between stratum}
\usage{
  stratum.distance(pop, stratum, lat = "Latitude",
    lon = "Longitude",
    mode = c("Great Circle", "Euclidean")[1], subset)
}
\arguments{
  \item{pop}{A \code{Population} object}

  \item{stratum}{The stratum to use for estimating
  distances}

  \item{lat}{The label for latitude (default 'Latitude')}

  \item{lon}{The label for longitude (default 'Longitude')}

  \item{mode}{Option for distance estimation.  "Great
  Circle" (default) takes into consideration the curvature
  of the earth, "Eucklidean" takes straigh-line distance.}

  \item{subset}{A list of stratum to use (default is
  missing and all)}
}
\value{
  A matrix of pair-wise distances (in km)
}
\description{
  This returns a matrix of physical distances among strata
  from a \code{Population}.
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}
\seealso{
  \code{\link{great.circle.distance}}
}

