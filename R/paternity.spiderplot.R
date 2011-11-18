##############################################################################
# 							    paternity.spiderplot                            
#                                                                             
# 						    R.J. Dyer <rjdyer@@vcu.edu>                        
#                                                                             
##############################################################################

#' Creates a graphical depiction of paternity
#'
#' Simple plot of a paternity analysis results.  
#'
#' @param pop A population that has all the individuals.
#' @param paternity A 'paternity' object from \code{paternity} 
#' @param indID The column header for adults
#' @param X Heading for x coordinates
#' @param Y Heading for y coordinates 
#' @param ... Other options to pass to the plot function
#' @return Nothing
#' @note This function creates a plot of the inferred paternity cases
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#'

paternity.spiderplot <- function( pop=NULL, paternity=NULL, indID="IndID", X="lon", Y="lat", ... ) {
	if( is.null(pop) )
		stop("You need to have a valid population for this plot.")
	if(is.null(paternity))
		stop("You need some paternity results to call this function.")
	coords <- unique(cbind( pop[[X]], pop[[Y]] ))
	momX <- pop[ pop[[indID]]==474, ][[X]][1]
	momY <- pop[ pop[[indID]]==474, ][[Y]][1]
	# plot the adults
	plot( coords[,1], coords[,2], col=colors()[81], pch=16, ...)
	
	# plot the fathers
	offs <- names( paternity$paternity )
	for( off in offs ){
		dadArray <- paternity$paternity[[off]]
		dadNames <- names(dadArray)
		nDads <- length(dadNames)
		for( i in 1:nDads) {
			dad <- dadNames[i]
			dadCol <- gray( 1 - dadArray[i])
			dadX <- pop[ pop[[indID]]==dad,][[X]][1]
			dadY <- pop[ pop[[indID]]==dad,][[Y]][1]
			lines( c(momX,dadX), c(momY,dadY), col=dadCol )
		}
	}
	
	
	
	# plot the central mother
	points(momX,momY,pch=16,col="gray",cex=1.4)
	points(momX,momY,cex=1.4)
	
}
