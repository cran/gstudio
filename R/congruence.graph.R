#' Create a congruence graph from populations
#' 
#' This function takes two populations and creates a congruence graph (e.g., a
#'	graph that has an edge set consisting of those edges that both parental 
#'	graphs have in common).  The two parental populations \emph{must} have the
#'	the same named strata for this to work.
#'
#' @param popA A \code{Population} object
#' @param popB A \code{Population} object
#' @param stratum The stratum to use to define the congruence node set.
#' @param mode The method by which the weight of the congruence graph edge set
#'	is determined.  \code{mode='binary'} means that the congruence graph will
#'	have edge weights as 0/1.  \code{mode='ave'} standardizes the total length
#'	of each parental graph edge set and the congruence graph is weighed as the
#'	average of the two standardized edge set.
#' @param alpha The significance to test the graph edges on.
#' @param lat The name of the latitude variable to add to the graph (if present)
#' @param lon The name of the longitude variable to add to the graph (if present)
#' @return A congruence igraph object
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'
congruence.graph <- function(popA, popB, stratum, mode=c("binary","ave")[2],alpha=0.05, lat="Latitude", lon="Longitude"){
	if( !is(popA,"Population") || !is(popB,"Population") )
		stop("You need to pass population classes to this function")
	if( !(stratum %in% names(popA)) || !(stratum %in% names(popB) ) )
		stop("Both populations must have the same stratum to partition")
	if( missing(mode) || !(mode %in% c("binary","ave") ) )
		stop("mode must be either 'binary' or 'ave' see documentation")
	
	congPops <- intersect(  as.character( levels( popA[[stratum]] )), as.character( levels( popB[[stratum]] )) )
	if( length(congPops)<2 )
		stop("The two populations have fewer than two populations in common...")
	
	congA <- popA[ popA[[stratum]] %in% congPops,]
	congB <- popB[ popB[[stratum]] %in% congPops,]
	
	if( any( table(congA[[stratum]]) < 3) )
		stop("popA has less than 3 individuals in at least one pop, this is not good.")
	if( any( table(congB[[stratum]]) < 3 ) )
		stop("popB has less than 3 individuals in at least one pop, this is not good.")
	
	cat("creating first graph\n")
	graphA <- population.graph(congA,stratum=stratum,alpha=alpha,lat=lat,lon=lon)
	cat("creating second graph\n")
	graphB <- population.graph(congB,stratum=stratum,alpha=alpha,lat=lat,lon=lon)
	
	cat("making congruence graph\n")
	A.cong <- get.adjacency(graphA) * get.adjacency(graphB)
	
	if( mode=="ave") {
		A.a <- get.adjacency(graphA, attr="weight")
		A.b <- get.adjacency(graphB, attr="weight")
		
		A.a <- A.a / sum(A.a) * 0.5
		A.b <- A.b / sum(A.b) * 0.5
		A.cong <- A.cong + A.a + A.b
	}
	
	cong.graph <- graph.adjacency(A.cong, mode="undirected", weighted=TRUE )
	cong.graph
	
}
