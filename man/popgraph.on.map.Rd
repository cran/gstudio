\name{popgraph.on.map}
\alias{popgraph.on.map}
\title{Creates google map with pie charts for allele frequencies.}
\usage{
  popgraph.on.map(graph, filename = "popgraph_on_map.kml")
}
\arguments{
  \item{graph}{A \code{Population} class (from gstudio
  package).  This graph must have \code{V(graph)$name},
  \code{V(graph)$latitude}, and \code{V(graph)$longitude}
  vertex attributes}

  \item{filename}{The name of the file to use
  (default=pie_on_map.kml)}
}
\description{
  This function creates a google map of the
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}
\seealso{
  \code{\link{pies.on.map}}
}

