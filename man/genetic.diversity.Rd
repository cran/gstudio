\name{genetic.diversity}
\alias{genetic.diversity}
\title{Estimates rarefied genetic diversity}
\usage{
  genetic.diversity(pop, stratum, loci,
    mode = c("A", "A95", "Ae")[3], num.perm = 999,
    verbose = FALSE)
}
\arguments{
  \item{pop}{A \code{Population} object}

  \item{stratum}{The stratum to use}

  \item{loci}{The genetic locus to use.}

  \item{mode}{The kind of diversity to estimate. Available
  types are A = the number of alleles, A95 = the number of
  alleles whose frequencies are at least 5 percent, and
  Ae=inverse of expected homozygosity.}

  \item{num.perm}{The number of permutations to use for
  rarefaction}

  \item{verbose}{A logical flag to indicate if the progress
  of the function should be portrayed on screen.}
}
\value{
  A genetic.diversity object.
}
\description{
  Uses rarefaction to estimate measures of genetic
  diversity that are standardized by sample size.
}

