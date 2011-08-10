\docType{methods}
\name{[-Locus-method}
\alias{[,Locus-method}
\alias{[<-,Locus-method}
\alias{[-Locus-method}
\alias{[<--Locus-method}
\title{Accessing elements within a locus object}
\description{
  This is an overload for [] indexing of alleles

  This is an overload for []<- indexing of alleles
}
\examples{
loc <- Locus( c("A","T") )
loc[1]
loc <- Locus( c("A","T") )
loc[1] <- "C"
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>
}

