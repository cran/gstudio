\docType{methods}
\name{column.class}
\alias{column.class}
\alias{column.class,Population-method}
\title{Returns the indices of the data columns that are of type
	requested}
\arguments{
  \item{pop}{A \code{Population} object}

  \item{type}{A character string as expected to be returned
  from the \code{class} function.}
}
\value{
  vector of column indices
}
\description{
  This is a quick helper function that returns the column
  indices for particular data types.
}
\examples{
\dontrun{

loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov1 <- rnorm(3)
cov2 <- rnorm(3)
pop <- Population( Pop=strata, env1=cov1, TPI=loci, env2=cov2 )
column.class( pop, "Locus" )

}
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

