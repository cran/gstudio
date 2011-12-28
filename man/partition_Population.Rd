\docType{methods}
\name{partition}
\alias{partition}
\alias{partition,Population-method}
\title{Returns a list of \code{Population} objects}
\arguments{
  \item{pop}{A \code{Population} object}

  \item{stratum}{A character vector of the stratum to
  partition on}
}
\value{
  A list of the \code{Populations} indexed by
  \code{stratum} name
}
\description{
  Returns a list of \code{Population} objects
}
\examples{
\dontrun{

loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
subpops <- partition(pop,"Pop")
print(subpops)

}
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

