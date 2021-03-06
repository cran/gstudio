% Generated by roxygen2 (4.0.1): do not edit by hand
\name{to_structure}
\alias{to_structure}
\title{Translates a \code{data.frame} with loci into a textual STRUCTURE file.}
\usage{
to_structure(df, stratum = "Population")
}
\arguments{
\item{df}{The \code{data.frame} to be written to the output.}

\item{stratum}{The stratum to use as "POP" (default="Population")}
}
\value{
A string representation of the \code{data.frame} formatted for STRUCTURE
}
\description{
This is a basic converter that takes a \code{data.frame} with \code{locus}
 objects in it and returns a textual representation as a STRUCTURE input
 file.
}
\examples{
A <- locus( c("1","1"))
B <- locus( c("1","2"))
C <- locus( c("2","2"))
loc1 <- c( A, A, B, B, B, B, C, C)
loc2 <- c( A, B, B, C, A, B, A, C)
pop <- data.frame( Population=c( rep("A",4),rep("B",4) ), loc1, loc2)
st <- to_structure( pop )
cat(st)
}
\author{
Rodney J. Dyer <rjdyer@vcu.edu>
}

