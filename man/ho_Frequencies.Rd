\docType{methods}
\name{ho}
\alias{ho}
\alias{ho,Frequencies-method}
\title{Gets observed heterozygosity}
\arguments{
  \item{freqs}{A \code{Frequencies} object}

  \item{size.corrected}{A logical flag to indicate that you
  want to correct the estimation of observed heterozygosity
  by the number of loci sampled}
}
\value{
  An estimate of observed heterozygosity
}
\description{
  Gets observed heterozygosity
}
\examples{
\dontrun{

loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) )
freqs <- Frequencies( loci )
ho( freqs, size.corrected=FALSE )
ho( freqs, size.corrected=TRUE )

}
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

