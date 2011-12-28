\docType{methods}
\name{summary}
\alias{summary}
\alias{summary,Frequencies-method}
\alias{summary,Locus-method}
\alias{summary,Population-method}
\title{Overload of \code{summary}}
\arguments{
  \item{object}{A \code{Locus} object}

  \item{object}{A \code{Population} object}

  \item{object}{A \code{Frequencies} object}
}
\value{
  A summary.Locus class

  A summary.Frequencies class
}
\description{
  This function is an overload of the \code{summary}
  function and returns the a rather boring summary of the
  locus

  Prints out a summary of the \code{Population}

  This function is an overload of the \code{summary}
  function for the \code{Frequencies} object.
}
\examples{
loc1 <- Locus( c("A","T") )
summary(loc1)
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
summary(pop)
loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) )
freqs <- Frequencies( loci )
summary(freqs)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>
}

