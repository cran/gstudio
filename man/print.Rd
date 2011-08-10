\docType{methods}
\name{print}
\alias{print,Frequencies-method}
\alias{print,Locus-method}
\alias{print}
\title{Printing a locus object}
\arguments{
  \item{x}{A \code{Locus} object}

  \item{x}{A \code{Frquencies} object}
}
\value{
  A character representation of the \code{Locus}

  A character representation of the \code{Frequencies}
  object
}
\description{
  This overloads the print function and prints the Locus

  This overloads the print function and prints the
  \code{Frequencies}
}
\examples{
loc <- Locus( c("A","T") )
print(loc)
loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) )
freqs <- Frequencies( loci )
print(freqs)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>
}

