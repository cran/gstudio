\docType{methods}
\name{dim}
\alias{dim}
\alias{dim,Population-method}
\title{Dimensions of the \code{Population}}
\arguments{
  \item{x}{A \code{Population} object}
}
\value{
  A two element vector with the number of rows and columns
}
\description{
  A length two vector of row and column sizes of the
  \code{Population}
}
\examples{
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(1,1)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
dim(pop)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

