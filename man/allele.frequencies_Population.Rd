\docType{methods}
\name{allele.frequencies}
\alias{allele.frequencies,Population-method}
\alias{allele.frequencies}
\title{Returns a \code{Frequencies} object (or objects)}
\arguments{
  \item{pop}{A \code{Population} object}

  \item{loci}{A character vector of locus names to use}
}
\value{
  A list of the \code{Frequencies} objects keyed by locus
  name
}
\description{
  Returns a \code{Frequencies} object (or objects)
}
\note{
  If nothing is supplied for \code{loci} then all loci are
  returned.
}
\examples{
\dontrun{

loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
freqs <- allele.frequencies(pop,"TPI")
freqs

}
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

