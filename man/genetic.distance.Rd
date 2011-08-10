\name{genetic.distance}
\alias{genetic.distance}
\title{Main function for estimating genetic distances}
\usage{
  genetic.distance(pop = NULL, stratum = NULL, loci = NULL,
  mode = c("Jaccard", "Bray", "AMOVA", "Euclidean",
  "Cavalli", "Nei", "cGD"))
}
\arguments{
  \item{pop}{A \code{Population} object}

  \item{stratum}{The stratum upon which partitions will be
  taken (required only for population-level analyses,
  ignored by individual distances).}

  \item{loci}{The name of the loci to use, if this argument
  is missing, all loci in the \code{Population} will be
  used.}

  \item{mode}{Which distance metric is to be used.  At
  present the following genetic distance metrics have been
  implemented.  "Jaccard set" dissimilarity of individuals
  based upon allelic states.  "Bray" Curtis distance
  denoting the proportion of shared allelic states between
  individuals.  "AMOVA" multilocus genetic distance among
  individuals.  "Euclidean" distance between population
  allele frequency vectors.  "Cavalli" -Sforza population
  allele frequency distance.  "Nei" 1977 uncorrected
  genetic distance among populations "cGD" Condiational
  genetic distance from the topology of a Population Graph}
}
\value{
  A list containing one to several distance matrices.  Some
  distance approaches are multilocus and for these only a
  single distance matrix will be return.  However, for
  others, there is no clear way to combine them (or there
  are several but I leave that up to you to choose) and a
  distance matrix for each locus is used.
}
\description{
  This is this main function that extimates genetic
  distances from a \code{\linkS4class{Population}} object.
}

