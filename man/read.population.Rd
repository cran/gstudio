\name{read.population}
\alias{read.population}
\title{Loads a population from a *.csv file}
\usage{
  read.population(file = NULL, googleURL = NULL,
    num.haploid = 0, header = TRUE, sep = ",",
    split = NULL)
}
\arguments{
  \item{file}{The path to the data file, this can be
  optional if you are loading a file from a google
  spreadsheet (see \code{googleURL} below).}

  \item{googleURL}{The URL to the shared spreadsheet}

  \item{num.haploid}{The number of that are haploid (or
  binary).  These will be loaded as the right most
  \code{num.haploid} number of columns.}

  \item{header}{A logical value indicating that the first
  line is column labels (defaults to TRUE).}

  \item{sep}{The character that separates columns in your
  data set (default=",")}

  \item{split}{This is an option (default=NULL) to split
  the indicated columns of genotypes into alleles.  This
  will take a genotype 33 and split it into \code{c(3,3)}
  for construction of the loci.  This is so that you don't
  have to do any odd scripting stuff to get your old
  genotype files into R (n.b., it will split in half for
  diploid, so 0101 will be turned into \code{ c("01","01")
  } ).  To use this, list the column numbers to split.}
}
\value{
  A fully formed \code{Population}
}
\description{
  General function to load a population from a file.
}
\note{
  The \code{numLoci} option can be optional IF the loci are
  denoted with a colon ':' (e.g., not haploid or binary).
  If you do not have markers of ploidy or greater, then you
  need to put the number of genetic loci here AND have the
  the loci as the right-most \code{numLoci} columns of the
  data file.  This means that NONE of your non-genetic
  columns can have a colon in them...
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

