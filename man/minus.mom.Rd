\name{minus.mom}
\alias{minus.mom}
\title{Removes the materinal contribution to offspring}
\usage{
  minus.mom(pop, indID = "IndID", offID = "OffID")
}
\arguments{
  \item{pop}{A population from which the maternal component
  will be removed.}

  \item{indID}{The column heading for the Individual ID
  number (Default="IndID")}

  \item{offID}{The column heading for the Offspring ID
  number (Default="OffID")}
}
\value{
  A \code{Population} of offspring that have reduced
  genotypes.
}
\description{
  This function removes the maternal contribution from an
  offspring by subtracting the
}
\note{
  When mother and offspring share the same heterozygote
  genotype then it is impossible to determine which allele
  the mother provided.  In these cases, the offspring
  genotype is not changed.  Also, in the designations of
  IndID and OffID, all adults must have unique IndID values
  but offspring must have IndID equal to the mother.  For
  OffID, all adults must have OffID=0 and their offspring
  have non-zero OffID values.  See the vignette on Parent
  Offspring data sets for a more complete discussion.
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

