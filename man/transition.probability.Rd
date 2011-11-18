\name{transition.probability}
\alias{transition.probability}
\title{Transition Probabilty for offspring given parents.}
\usage{
  transition.probability(mom, off, dad)
}
\arguments{
  \item{mom}{The Loci for the mother}

  \item{off}{The Loci for the offspring}

  \item{dad}{A set of father loci (by row) that may be dads
  of the offspring}
}
\value{
  Mendelian transisiton probability for offspring given
  parents (a value of zero is assigned for parent pairs
  that are impossible)
}
\description{
  Simple function that estimates basic paternity exclusion
  for a single maternal family.
}
\note{
  This requires that loci are diploid
}
\examples{
mom <- Locus( c("A","B") )
dad <- Locus( c("B","B") )
off <- Locus( c("A","B"))
transition.probability(mom,off,dad)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

