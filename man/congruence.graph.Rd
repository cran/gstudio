\name{congruence.graph}
\alias{congruence.graph}
\title{Create a congruence graph from populations}
\usage{
  congruence.graph(popA, popB, stratum, mode = c("binary",
  "ave")[2], alpha = 0.05, lat = "Latitude", lon =
  "Longitude")
}
\arguments{
  \item{popA}{A \code{Population} object}

  \item{popB}{A \code{Population} object}

  \item{stratum}{The stratum to use to define the
  congruence node set.}

  \item{mode}{The method by which the weight of the
  congruence graph edge set is determined.
  \code{mode='binary'} means that the congruence graph will
  have edge weights as 0/1.  \code{mode='ave'} standardizes
  the total length of each parental graph edge set and the
  congruence graph is weighed as the average of the two
  standardized edge set.}

  \item{alpha}{The significance to test the graph edges
  on.}

  \item{lat}{The name of the latitude variable to add to
  the graph (if present)}

  \item{lon}{The name of the longitude variable to add to
  the graph (if present)}
}
\value{
  A congruence igraph object
}
\description{
  This function takes two populations and creates a
  congruence graph (e.g., a graph that has an edge set
  consisting of those edges that both parental graphs have
  in common).  The two parental populations \emph{must}
  have the the same named strata for this to work.
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

