\docType{methods}
\name{row.names}
\alias{row.names,Population-method}
\alias{row.names}
\title{Overload row.names function for \code{Population}}
\arguments{
  \item{x}{A \code{Population} object}
}
\value{
  Row names for the population
}
\description{
  Returns a list of row names for the \code{Population}
}
\note{
  This is kind of a stupid function in its present form,
  there are no specific row names in a \code{Population}
  object, you only index rows by number.  So this just
  returns a 1:nRows numeric array...
}
\examples{
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
row.names(pop)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

