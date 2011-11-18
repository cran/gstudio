##############################################################################
# 							    offspring.array                             
#                                                                             
# 						    R.J. Dyer <rjdyer@@vcu.edu>                        
#                                                                             
##############################################################################

#' Grabs mothers and offspring from a larger data set 
#'
#' This is a convienence function that grabs mothers and offspring from an 
#'	existing data set, returning them as a list.
#'
#' @param pop A \code{Population} of individuals that contains both mothers
#'	and offspring.
#' @param momID The identification of the maternal individual.
#' @param idCol The name of the column that has the individual id number (n.b.,
#'	all offspring have the individual id of the mother)  Default="IndID"
#' @param offCol The name of the column that has the offspring id number (n.b., 
#'	the mother has offID=0 and all offspring have non-zero offspring ids)
#' @return A list with keys "mom" & "offspring" 
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export

offspring.array <- function(pop,momID,idCol="IndID",offCol="OffID"){
	if( !is(pop,"Population") )
		stop("Need to pass a Population object to this function.")
		
	col <- which(names(pop)==idCol)
	family <- pop[pop[,col]==momID,]
	if( dim(family)[1]<1)
		stop(paste("No mother with id",momID,"in the data set."))
	
	col <- which(names(family)==offCol)
	mom <- family[ family[,col]==0,]
	offspring <- family[ family[,col]!=0,]
	return( list(mom=mom,offspring=offspring))
}
