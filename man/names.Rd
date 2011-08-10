\docType{methods}
\name{names}
\alias{names,Frequencies-method}
\alias{names,Population-method}
\alias{names}
\title{Data column names in \code{Population} a locus object}
\arguments{
  \item{x}{A \code{Population} object}

  \item{x}{A \code{Frquencies} object}
}
\value{
  A vector of column names or NULL if empty

  A character list of alleles
}
\description{
  This overloads the names function and prints the data
  column names

  This overloads the names function and returns the alleles
  in the \code{Frequencies} object
}
\examples{
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(1,1)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
names(pop)
loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) )
freqs <- Frequencies( loci )
names(freqs)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>
}

