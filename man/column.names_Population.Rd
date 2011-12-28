\docType{methods}
\name{column.names}
\alias{column.names}
\alias{column.names,Population-method}
\title{Returns data names for specific kind of class}
\arguments{
  \item{pop}{A \code{Population} object}

  \item{type}{The \code{class} of data column of interest.}
}
\value{
  A list of the names of \code{class(type)} in the
  \code{Population}
}
\description{
  Returns data names for specific kind of class
}
\examples{
\dontrun{

loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
column.names(pop,type="character")
column.names(pop,type="Locus")
column.names(pop,type="factor")

}
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

