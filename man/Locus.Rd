\name{Locus}
\alias{Locus}
\title{Constructor function for \code{\linkS4class{Locus}} objects}
\usage{
  Locus(alleles = character(0))
}
\arguments{
  \item{alleles}{A vector of objects that can be turned
  into \code{character}}
}
\value{
  A fully formed \code{\linkS4class{Locus}} object.
}
\description{
  Locus class description
}
\details{
  This function will not sort the alleles but will keep
  them in normal order
}
\note{
  Missing data can be encoded as either '-9' or 'NA'.  If
  there is a single missing allele at the locus, then the
  entire locus will be considered as missing.  If you are
  mixing diploid and haploid individuals, encode the
  haploids as only a single allele and do not try to put in
  the extra allele as missing.
}
\examples{
loc <- Locus( 1:3 )
loc <- Locus( c("A","T"))
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>
}
\seealso{
  \code{\linkS4class{Locus}}
}

