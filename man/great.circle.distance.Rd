\name{great.circle.distance}
\alias{great.circle.distance}
\title{Get the "great circle" distance (in km) between two points}
\usage{
  great.circle.distance(lon1, lat1, lon2, lat2)
}
\arguments{
  \item{lon1}{Decimal longitude of first point}

  \item{lat1}{Decimal latitude of first point}

  \item{lon2}{Decimal longitude of the second point}

  \item{lat2}{Decimal latitude of the second point}
}
\value{
  Great circle distance between points in km.
}
\description{
  Calculates the distance between two points given as
  latitude and longitude using the law of inverse sines
  approach.
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

