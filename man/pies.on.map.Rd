\name{pies.on.map}
\alias{pies.on.map}
\title{Creates google map with pie charts for allele frequencies.}
\usage{
  pies.on.map(filename = "pies_on_map.kml", pop = NULL,
  stratum = NULL, loci = NULL, lat = "Latitude", lon =
  "Longitude")
}
\arguments{
  \item{filename}{The name of the file to use
  (default=pie_on_map.kml)}

  \item{pop}{A \code{Population} class (from gstudio
  package)}

  \item{stratum}{The stratum to plot}

  \item{loci}{The locus to use}

  \item{lat}{The label for the latitude data column in pop
  if not "lat}

  \item{lon}{The label for the longitude data column in po
  if not "lon}
}
\description{
  This function creates a google map of the
}

