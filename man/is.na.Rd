\docType{methods}
\name{is.na}
\alias{is.na,Locus-method}
\alias{is.na}
\title{Overload of \code{is.na}}
\arguments{
  \item{x}{A \code{Locus} object}
}
\value{
  A logical indicating that the locus is empty
}
\description{
  This function is an overload of the \code{is.na} function
  and returns a logical indicating if the locus is empty.
}
\examples{
loc1 <- Locus( c("A","T") )
is.na(loc1)
loc2 <- Locus( )
is.na(loc2)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

