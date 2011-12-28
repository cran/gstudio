\docType{methods}
\name{get.frequencies}
\alias{get.frequencies}
\alias{get.frequencies,Frequencies-method}
\title{Gets allele frequencies from pop}
\arguments{
  \item{freqs}{A \code{Frequencies} object}

  \item{alleles}{A list of alleles}
}
\value{
  A vector of allele frequencies
}
\description{
  Gets allele frequencies from pop
}
\examples{
\dontrun{

loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) )
freqs <- Frequencies( loci )
get.frequencies( freqs, c(1,2,3) )

}
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

