#' Calucluate pair-wise distance between stratum
#'
#' This returns a matrix of physical distances among strata from a 
#'	\code{Population}.
#'
#' @param pop A \code{Population} object
#' @param stratum The stratum to use for estimating distances
#' @param lat The label for latitude (default 'Latitude')
#' @param lon The label for longitude (default 'Longitude')
#' @param subset A list of stratum to use (default is missing and all)
#' @return A matrix of pair-wise distances (in km)
#' @seealso \code{\link{great.circle.distance}}
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#'
stratum.distance <- function(pop,stratum,lat="Latitude",lon="Longitude",subset) {
	if( missing(pop) || missing(stratum) )
		stop("Both pop and stratum are required for stratum.distance")
	
	
	strata <- partition(pop,stratum)		
	if(!missing(subset) ){
		strata <- strata[subset]
	}

	popnames <- names(strata)
	K <- length(strata)
	if( K < 2 )
		stop("You need at least two strata to estimate physical distance.")

	D <- matrix(0,K,K)
	row.names(D) <- popnames
	colnames(D) <- popnames
	
	for(i in 1:K){
		lat1 <- mean(pop[pop[[stratum]]==popnames[i]][[lat]])
		lon1 <- mean(pop[pop[[stratum]]==popnames[i]][[lon]])

		for(j in (i+1):K){
			if(j<=K){
				lat2 <- mean(pop[pop[[stratum]]==popnames[j]][[lat]])
				lon2 <- mean(pop[pop[[stratum]]==popnames[j]][[lon]])
				d <- great.circle.distance(lon1,lat1,lon2,lat2)
				D[i,j] <- D[j,i] <- d
			}
		}
	}
	D
}