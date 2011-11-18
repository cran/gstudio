##############################################################################
# 							    exclusion.probability                            
#                                                                             
# 						    R.J. Dyer <rjdyer@@vcu.edu>                        
#                                                                             
##############################################################################

#' Estimates the exclusion probability from a Frequencies object
#'
#' 
#'
#' @param freq A Frequencies object calculated from pollen pool allele frequencies
#'	or from a set of adults.
#' @return Single locus exclusion probability
#' @note This function estiamtes the single-locus exclusion probability.  It is left 
#'	up to you to decide what group of individuals you are going to use to estimate 
#'	background allele frequencies.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export

exclusion.probability <- function( freq ) {
	
	p <- get.frequencies( freq )
	p1 <- 0.0
	p2 <- 0.0
	l <- length(p)
	
	# first part
	for( i in 1:l ){
		p1 <- p1 + p[i]*(1-p[i])^2
		for(j in 1:l){
			if( i!=j ){
				p2 <- p2 + p[i]^2 * p[j]^2 * (4 - 3*p[i] - 3*p[j])
			}
		}
	}
	pe <- p1 - 0.5*p2
	names(pe) <- "Pe"
	return(pe)
}

