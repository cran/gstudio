#' Probability for congruence graph
#'
#' Estimates the cumulative probability function for observing a 
#'	congruence graph based upon the size of the edge sets in both
#' 	parental graphs and the resulting congruence graph.
#' 
#' @param graphA First population graph
#' @param graphB Second population graph
#' @param congGraph The congruence graph made from \code{graphA}
#'	and \code{graphB}.
#' @param as.cdf A logical switch indicating that you want the
#'	answer as the raw probability (the default) or the cumulative 
#'	density function from congruence graphs of size 0:E(congGraph)
#' @return Either the raw probability of the congruence graph (e.g.,
#'	the fraction of congruence graphs with as many or fewer edges than
#'	observed in \code{congGraph}) or the CDF of the probability.
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'
congruence.probability <- function(graphA, graphB, congGraph, as.cdf=FALSE ) {
	N <- length(V(graphA))
	if( !N )
		stop("Cannot work with graphs without edges")
	if( !is(graphA,"igraph") || !is(graphB,"igraph") || !is(congGraph,"igraph") )
		stop("All arguments must be graphs")
	if( sort(V(graphA)$name) != sort(V(graphB)$name) ||
		sort(V(graphB)$name) != sort(V(congGraph)$name) )
		stop("All three graphs must have the same edge set.")
	if( N != length(V(graphB)) || N != length(V(congGraph)) )
		stop("All graphs need to have the same sized node set to test congurence")
	
	mA <- length(  E( graphA ) )
	mB <- length(  E( graphB ) )
	mC <- 0:length(E( congGraph ) )

	mMax <- N*(N-1)/2
	ell = mA + mB - mMax
	i <- max(0,ell):mA
	
	p.top <- choose( mA,mC ) * choose( mMax-mA, mB-mC )
	p.bot <- sum( choose(mA,i) * choose( mMax-mA, mB-i ))
	
	if( as.cdf)
		return( data.frame( CongEdgeSetSize=mC, Prob=p.top/p.bot ) )
	else
		return( sum( p.top/p.bot ) )
}
