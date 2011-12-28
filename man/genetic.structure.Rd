\name{genetic.structure}
\alias{genetic.structure}
\title{Main function for estimating genetic structure parameters}
\usage{
  genetic.structure(pop = NULL, stratum = NULL,
    loci = NULL, mode = c("Gst", "Gst.prime", "Dest"),
    num.perm = 0, verbose = FALSE, pairwise = FALSE)
}
\arguments{
  \item{pop}{A \code{\linkS4class{Population}} object}

  \item{stratum}{The column on which individuals will be
  partitioned}

  \item{loci}{The locus or loci that the parameters will be
  estimated (see note below).  If this option is not given,
  all loci will be used.}

  \item{mode}{The parameter that is to be estimated.
  Current options include "Gst", "Gst.prime", and "Dest".}

  \item{num.perm}{The number of permutations to used for
  significance testing.  All significance is tested using
  permutation of individuals.}

  \item{verbose}{A logical flag to indicate the locus
  progress (default=FALSE)}

  \item{pairwise}{A logical flag to indicate that the
  analysis should be done in a pairwise fasion and return
  the results as a matrix instead of as a genetic.structure
  object.}
}
\value{
  A \code{genetic.structure} object that has overloaded
  \code{print}, \code{summary}, and \code{plot} functions.
}
\description{
  Main function for estimating genetic structure parameters
}
\note{
  For multilocus estimates of these parameters, both
  individual single locus and a multilocus parameter will
  be provied for \code{mode} types \eqn{G_{ST}},
  \eqn{G_{ST}^\prime}, and \eqn{D_{est}}.  Of note, for
  \eqn{G_{ST}}, \eqn{G_{ST}^\prime}, and \eqn{D_{est}} the
  numerator and denominantor of the multilocus parameter is
  accumulated across loci before division.  You cannot just
  average single locus estimates or you can get a vastly
  different result (see REF).
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu
}
\references{
  Joost, Hedrick, averaging gst one,
}
\seealso{
  \code{\link{print.genetic.structure}},
  \code{\link{plot.genetic.structure}}
}

