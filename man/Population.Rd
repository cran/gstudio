\name{Population}
\alias{Population}
\title{Constructor function for \code{\linkS4class{Population}} objects}
\usage{
  Population(...)
}
\arguments{
  \item{...}{A variable number of vectors and lists that
  can be passed to the Population. All parameters have to
  be named (e.g. x=rnorm(20), etc) and cannot be anonymous}
}
\value{
  A fully formed \code{\linkS4class{Population}} object.
}
\description{
  Locus class description
}
\details{
  This function will not sort the alleles but will keep
  them in normal order
}
\note{
  This is a general data.frame-like object that can easily
  hold strata, covariates, and \code{\linkS4class{Locus}}
  objects.  You can append data columns onto this after it
  has been constructed but you cannot append rows onto it.
}
\examples{
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(1,1)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
pop
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>
}
\seealso{
  \code{\linkS4class{Locus}}
}

