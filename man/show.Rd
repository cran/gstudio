\docType{methods}
\name{show}
\alias{show,Frequencies-method}
\alias{show,Locus-method}
\alias{show,Population-method}
\alias{show}
\title{Showing a locus object}
\arguments{
  \item{x}{A \code{Locus} object}

  \item{...}{Ignored}

  \item{x}{A \code{Population} object}

  \item{x}{A \code{Frequencies} object}
}
\description{
  This overloads the show function and prints the Locus

  Prints out the data within the \code{Population}

  This overloads the show function and prints the
  \code{Frequencies}
}
\examples{
loc <- Locus( c("A","T") )
loc
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(1,1)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
pop  ## equivalent to show(pop)
loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) )
freqs <- Frequencies( loci )
freqs  ## equivalent to show(freqs)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>
}

