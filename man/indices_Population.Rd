\docType{methods}
\name{indices}
\alias{indices,Population-method}
\alias{indices}
\title{Returns the numeric indices of data column names}
\arguments{
  \item{pop}{A \code{Population} object}

  \item{names}{A character vector of the stratum}
}
\value{
  A vector of numeric column indices
}
\description{
  Returns the numeric indices of data column names
}
\examples{
\dontrun{

loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov1 <- rnorm(3)
cov2 <- rnorm(3)
pop <- Population( Pop=strata, env1=cov1, TPI=loci, env2=cov2 )
indices( pop, c("env1","env2"))

}
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

