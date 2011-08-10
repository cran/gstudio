\docType{methods}
\name{as.vector}
\alias{as.vector,Locus-method}
\alias{as.vector}
\title{Overload of \code{as.vector}}
\arguments{
  \item{x}{A \code{Locus} object}

  \item{mode}{A vector of allele states}
}
\value{
  A multivariate version of the \code{Locus}
}
\description{
  This function is an overload of the \code{as.vector}
  function and returns the locus as a multivariate vector
  whose alleles are indexed by the vector of alleles you
  pass via \code{mode}
}
\examples{
loc <- Locus( c("A","T") )
as.character(loc)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

