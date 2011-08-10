\docType{methods}
\name{as.character}
\alias{as.character,Locus-method}
\alias{as.character}
\title{Overload of \code{as.character} function}
\arguments{
  \item{x}{A \code{Locus} object}

  \item{...}{Ignored}
}
\value{
  A character version of the \code{Locus}
}
\description{
  Returns character version of the \code{Locus}
}
\examples{
loc <- Locus( c("A","T") )
as.character(loc)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

