\name{paternity}
\alias{paternity}
\title{Performs paternity exclusion using Fractional analysis}
\usage{
  paternity(pop, familyID = NULL, indCol = "IndID", offCol
  = "OffID")
}
\arguments{
  \item{pop}{A population that has all the individuals.}

  \item{familyID}{The IndID for the family (see notes).}

  \item{indCol}{Heading for IndID column in
  \code{Population} (see note)}

  \item{offCol}{Heading for OffID column in
  \code{Population} (see note)}
}
\value{
  A \code{paternity} object.
}
\description{
  Simple function that estimates basic paternity exclusion
  for a family array using fractional paternity from
  \code{transition.probability}
}
\note{
  The form of a mixed offspring/parent \code{Population}
  has to have at least the following.  All adults must have
  a unique ID number that will be indexed by the argument
  indCol (default='IndID').  Offspring for a particular
  maternal individual all have the same indCol, that of the
  mother.  A second identification column is also required,
  indexed by offCol (default='OffID'), that is used to
  differentiate among half-sibs within a maternal offspring
  array.  Mothers must have OffID=0 and all of her
  offspring will have a non-zero value for this. This
  approach allow you to have all your data kept in a single
  \code{Population}.
}
\examples{
data(cornus_florida)
f <- paternity( cornus_florida, familyID=474)
print(f)
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}
\seealso{
  \code{offspring.array}, \code{transition.probability}
}

