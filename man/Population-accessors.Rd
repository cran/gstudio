\docType{methods}
\name{names<-}
\alias{[,Population-method}
\alias{[[,Population-method}
\alias{[<-,Population-method}
\alias{$,Population-method}
\alias{$<-,Population-method}
\alias{names<-,Population-method}
\alias{[-Population-accessors}
\alias{[[-Population-accessors}
\alias{[<--Population-accessors}
\alias{$}
\alias{$<-}
\alias{append}
\alias{names<-}
\title{Set names of data columns in the \code{Population}}
\arguments{
  \item{x}{A \code{Population} object}

  \item{value}{The new names}

  \item{x}{A \code{Population}}

  \item{values}{Another \code{Population}}
}
\value{
  Nothing

  It depends upon how you call this one, it can return a
  whole column (all the same) data types if you do
  something like \code{pop[,1]}, it can return a row (an
  individual) if your indexes are \code{pop[1,]} or it can
  return an individual element if you use both indexes
  \code{pop[1,2]}.

  A list of values for the column

  A \code{Population} with all data from both x and values
}
\description{
  This overloads the names()<- function to reset the names
  of the data columns in the \code{Population}

  This is an overload for $ indexing data columns in a
  \code{Population}

  This is an overload for $<- to set entire data columns in
  a \code{Population}

  This is an overload for [ indexing data columns in a
  \code{Population} by numerical index instead of by name.
  This allows you to get rows, slices, or use other logical
  operators to get the subset of the \code{Population} that
  you want.

  This is an overload for [<- to set entire data columns in
  a \code{Population} by numeric index

  This is an overload for [[ indexing data columns in a
  \code{Population} by name or names.  This essentially
  calls the [,idx] function.

  This is an overload function of append that will add
  individuals onto the end of the population.
}
\note{
  This function is the main way to add additional columns
  of data onto an existing \code{Population} object.  Just
  make sure that the named column does not exhist
  already...

  You can do some interesting slicing with this function.
  TODO: add more examples.

  This function can replace an element of the
  \code{Population} (e.g., \code{pop[2,3] <- 42}), an
  entire data column (\code{pop[,1] <- rnorm(10)}), or an
  entire row (\code{ pop[1,] <- newInd}; as long as the
  \code{newInd} is a list with the same exact names as the
  pop).  If you to replace an element or row that is of a
  different \code{class}, the code will barf and not let
  you... (unless you put a column as \code{NULL} as that is
  the default way to remove it in \code{data.frames}).

  Only returns a single column.

  The two populations must have the same data columns, and
  in the same order.
}
\examples{
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(1,1)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
nm <- names(pop)
nm[2] <- "Environment"
names(pop) <- nm
names(pop)
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
pop$env
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
summary(pop)
pop$elevation <- runif(3)
summary(pop)
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
pop[,1]
pop[pop$Pop!="Loreto",]
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
pop[1,1] <- "Loreto"
pop
pop[,2] <- rep(0,3)
pop
loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
strata <- c("Cabo","Cabo","Loreto")
cov <- rnorm(3)
pop <- Population( Pop=strata, env=cov, TPI=loci )
popLabel <- "Pop"
pop[[popLabel]]
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>

  Rodney J. Dyer <rjdyer@vcu.edu>
}

