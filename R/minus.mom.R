##############################################################################
# 							    minus.mom                             
#                                                                             
# 						    R.J. Dyer <rjdyer@@vcu.edu>                        
#                                                                             
##############################################################################

#' Removes the materinal contribution to offspring
#'
#' This function removes the maternal contribution from an offspring by 
#'	subtracting the 
#'
#' @param pop A population from which the maternal component will be removed.
#' @param indCol dataThe column heading for the Individual ID number (Default="IndID")
#' @param offCol The column heading for the Offspring ID number (Default="OffID")
#' @return A \code{Population} of offspring that have reduced genotypes.
#' @note When mother and offspring share the same heterozygote genotype then 
#'	it is impossible to determine which allele the mother provided.  In these
#'	cases, the offspring genotype is not changed.  Also, in the designations of
#'	IndID and OffID, all adults must have unique IndID values but offspring must
#'	have IndID equal to the mother.  For OffID, all adults must have OffID=0 and 
#' 	their offspring have non-zero OffID values.  See the vignette on Parent Offspring
#'	data sets for a more complete discussion.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export

minus.mom <- function( pop, indCol="IndID", offCol="OffID" ){
	loci <- column.class( pop, "Locus" )
	if( !length(loci))
		stop("You need to have some genetic data if you want to pull it apart.")

	offspring <- pop[ pop[[offCol]] != 0, ]				
	K <- length(offspring[,1])
	for(i in 1:K) {
		famID = offspring[[indCol]][i]
		mom = pop[ pop[[indCol]]==famID & pop[[offCol]]==0, ]
		if( length(mom) == length(pop) & length(mom[,1]) ){
			for( col in loci ) {
				offspring[i,col] <- offspring[i,col] - mom[1,col]
			}
		}
	}
	return(offspring)
}
