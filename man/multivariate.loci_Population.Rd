\docType{methods}
\name{multivariate.loci}
\alias{multivariate.loci,Population-method}
\alias{multivariate.loci}
\title{Returns multivariate genotypes for all loci}
\arguments{
  \item{pop}{A \code{Population} object}

  \item{loci}{The loci to use.  If this parameter is
  missing then all loci will be used}

  \item{ploidy}{This is a multiplication factor for the
  coding of individual alleles.  The \code{Locus} class
  method \code{as.vector} returns counts of alleles whose
  \code{sum{as.vector(locus)}=ploidy} but there are times
  when you do not want to have the presence of an allele=1
  but rather are interested in the freqeuncy of the allele
  at the locus (e.g. should be 0.5 for a diploid
  individual).  Set ploidy=1 for multivariate.loci to be
  counts (the default setting) or to the ploidy of the
  locus (e.g., ploidy=2) for frequencies at the locus.}
}
\value{
  A multivariate matrix where all genotypes will be turned
  into a single concatinated vector.
}
\description{
  Returns multivariate genotypes for all loci
}
\examples{
\dontrun{

loc1 <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
loc2 <- c( Locus(c(5,5)),Locus(c(5,7)),Locus(c(5,7)) )
strata <- c("Cabo","Cabo","Loreto")
pop <- Population( Pop=strata, TPI=loc1, PGM=loc2 )
multivariate.loci(pop)

}
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}
\seealso{
  \code{\link{as.vector}}
}

