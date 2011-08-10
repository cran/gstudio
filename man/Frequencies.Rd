\name{Frequencies}
\alias{Frequencies}
\title{Constructor function for \code{\linkS4class{Frequencies}} objects}
\usage{
  Frequencies(loci = list())
}
\arguments{
  \item{loci}{A list of \code{Locus} objects.  Can be
  empty}
}
\value{
  A fully formed \code{\linkS4class{Frequencies}} object.
}
\description{
  This function will not sort the alleles but will keep
  them in normal order
}
\examples{
loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) )
freqs <- Frequencies( loci )
freqs
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}
\seealso{
  \code{\linkS4class{Locus}},
  \code{\linkS4class{Population}}
}

