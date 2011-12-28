\name{offspring.array}
\alias{offspring.array}
\title{Grabs mothers and offspring from a larger data set}
\usage{
  offspring.array(pop, momID, idCol = "IndID",
    offCol = "OffID")
}
\arguments{
  \item{pop}{A \code{Population} of individuals that
  contains both mothers and offspring.}

  \item{momID}{The identification of the maternal
  individual.}

  \item{idCol}{The name of the column that has the
  individual id number (n.b., all offspring have the
  individual id of the mother) Default="IndID}

  \item{offCol}{The name of the column that has the
  offspring id number (n.b., the mother has offID=0 and all
  offspring have non-zero offspring ids)}
}
\value{
  A list with keys "mom" & "offspring"
}
\description{
  This is a convienence function that grabs mothers and
  offspring from an existing data set, returning them as a
  list.
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

