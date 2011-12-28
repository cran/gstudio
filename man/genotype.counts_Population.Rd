\docType{methods}
\name{genotype.counts}
\alias{genotype.counts}
\alias{genotype.counts,Population-method}
\title{Returns tally of genotypes}
\arguments{
  \item{pop}{A \code{Population} object}

  \item{loci}{The locus to use.}
}
\value{
  A numeric vector with names corresponding to genotypes
}
\description{
  Returns tally of genotypes
}
\examples{
\dontrun{

loc1 <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
pop <- Population( Pop=strata, TPI=loc1 )
genotype.counts(pop,"TPI")

}
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}
\seealso{
  \code{\link{as.vector}}
}

