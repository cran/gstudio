\name{population.graph}
\alias{population.graph}
\title{Create a Population Graph from a data set}
\usage{
  population.graph(pop, stratum = NULL, loci = NULL, alpha
  = 0.05)
}
\arguments{
  \item{pop}{A \code{Population} object}

  \item{stratum}{The stratum that you want to represent the
  node set}

  \item{loci}{The subset of loci to use. If this is not
  given then all loci in the population will be used (the
  default).}

  \item{alpha}{The alpha level at which you want the edges
  to be tested.}
}
\value{
  An igraph object of the population graph with node names
  and sizes as vertex attributes.
}
\description{
  Takes a population and creates a population graph
  topology in terms of an adjacency matrix.
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

