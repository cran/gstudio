\docType{methods}
\name{he}
\alias{he,Frequencies-method}
\alias{he}
\title{Gets expected heterozygosity}
\arguments{
  \item{freqs}{A \code{Frequencies} object}

  \item{size.corrected}{A logical flag to indicate that you
  want to correct the estimation of observed heterozygosity
  by the number of loci sampled}
}
\value{
  An estimate of expected heterozygosity
}
\description{
  Gets expected heterozygosity
}
\examples{
\dontrun{

loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) )
freqs <- Frequencies( loci )
he( freqs, size.corrected=FALSE )
he( freqs, size.corrected=TRUE )

}
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

