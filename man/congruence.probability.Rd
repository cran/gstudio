\name{congruence.probability}
\alias{congruence.probability}
\title{Probability for congruence graph}
\usage{
  congruence.probability(graphA, graphB, congGraph,
    as.cdf = FALSE)
}
\arguments{
  \item{graphA}{First population graph}

  \item{graphB}{Second population graph}

  \item{congGraph}{The congruence graph made from
  \code{graphA} and \code{graphB}.}

  \item{as.cdf}{A logical switch indicating that you want
  the answer as the raw probability (the default) or the
  cumulative density function from congruence graphs of
  size 0:E(congGraph)}
}
\value{
  Either the raw probability of the congruence graph (e.g.,
  the fraction of congruence graphs with as many or fewer
  edges than observed in \code{congGraph}) or the CDF of
  the probability.
}
\description{
  Estimates the cumulative probability function for
  observing a congruence graph based upon the size of the
  edge sets in both parental graphs and the resulting
  congruence graph.
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

