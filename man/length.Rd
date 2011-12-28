\docType{methods}
\name{length}
\alias{length}
\alias{length,Frequencies-method}
\alias{length,Locus-method}
\alias{length,Population-method}
\title{Overload of \code{length}}
\arguments{
  \item{x}{A \code{Locus} object}

  \item{x}{A \code{Population} object}

  \item{x}{A \code{Frquencies} object}
}
\value{
  The number of alleles

  The number of data columns

  The number of alleles observed
}
\description{
  This function is an overload of the \code{length}
  function and returns the number of alleles at the locus
  (e.g., the ploidy).

  The length of a \code{Population} is defined as the
  number of data columns

  This overloads the length function for
  \code{Frequencies}.  It returns the number of alleles in
  the object.
}
\examples{
loc1 <- Locus( c("A","T") )
length(loc1)
loc2 <- Locus( 1:4 )
length(loc2)
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(1,1)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
length(pop)
loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) )
freqs <- Frequencies( loci )
length(freqs)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>
}

