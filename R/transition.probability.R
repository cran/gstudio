##############################################################################
# 							    transition.probabilty                            
#                                                                             
# 						    R.J. Dyer <rjdyer@@vcu.edu>                        
#                                                                             
##############################################################################

#' Transition Probabilty for offspring given parents.
#'
#' Simple function that estimates basic paternity exclusion for a single maternal
#'	family.  
#'
#' @param mom The Loci for the mother 
#' @param off The Loci for the offspring 
#' @param dad A set of father loci (by row) that may be dads of the offspring
#' @return Mendelian transisiton probability for offspring given parents (a value
#'	of zero is assigned for parent pairs that are impossible)
#' @note This requires that loci are diploid
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#' @examples
#'  
#' mom <- Locus( c("A","B") )
#' dad <- Locus( c("B","B") )
#' off <- Locus( c("A","B"))
#' transition.probability(mom,off,dad)
#'

transition.probability <- function(mom,off,dad){
	if(!is(mom,"Locus") | !is(off,"Locus") | !is(dad,"Locus") )
		stop("This function requires individual loci to be passed for all arguments")
	if( length(mom) != 2 | length(off) != 2 | length(dad) != 2 )
		stop("This function only handles diploid loci at this point.")

	mm <- off - mom
	if( !any(mm@alleles %in% dad@alleles) )
		return(0.0)
	mp <- ifelse(is.heterozygote(mom),0.5,1.0)
	dp <- ifelse(is.heterozygote(dad),0.5,1.0)
	op <- ifelse( (mom==dad & mom==off & is.heterozygote(off)), 2, 1 )

	return(mp*dp*op)
}

