##############################################################################
# 							      genetic.distance                           #
#                                                                            #
# 						    R.J. Dyer <rjdyer@@vcu.edu>                      #
#                                                                            #
##############################################################################


#' Main function for estimating genetic distances
#'
#' This is this main function that extimates genetic distances from a 
#' \code{\linkS4class{Population}} object.
#'
#' @param pop A \code{Population} object
#' @param stratum The stratum upon which partitions will be taken (required 
#'	only for population-level analyses, ignored by individual distances).
#' @param loci The name of the loci to use, if this argument is missing, all 
#'	loci in the \code{Population} will be used.
#' @param mode Which distance metric is to be used.  At present the following 
#'	genetic distance metrics have been implemented.  "Jaccard set" dissimilarity 
#'	of individuals based upon allelic states.  "Bray" Curtis distance denoting 
#'	the proportion of shared allelic states between individuals.  "AMOVA" 
#'	multilocus genetic distance among individuals.  "Euclidean" distance between 
#'	population allele frequency vectors.  "Cavalli" -Sforza population allele 
#'	frequency distance.  "Nei" 1977 uncorrected genetic distance among populations
#'	"cGD" Condiational genetic distance from the topology of a Population Graph
#' @return A list containing one to several distance matrices.  Some distance
#'	approaches are multilocus and for these only a single distance matrix will
#'	be return.  However, for others, there is no clear way to combine them (or
#'	there are several but I leave that up to you to choose) and a distance matrix
#'	for each locus is used.
#' @export

genetic.distance <- function( 	pop=NULL,
	 							stratum=NULL,
								loci=NULL,
								mode=c("Jaccard","Bray","AMOVA",
										"Euclidean","Cavalli","Nei","cGD") ) 
{
	if( is.null(pop) ) 
		stop("Pop is a required parameters.")
	if( !is(pop,"Population") )
		stop("The pop parameter should be a Population object")
	if( is.null( loci ) )
		loci <- column.names(pop,"Locus")
	if( !length(loci) )
		stop("You need to have some loc in the population")
	if( length(mode)>1)
		stop("You need to supply one 'mode' for this function.")
	if( !(mode %in% c("Jaccard","Bray","AMOVA","Euclidean","Cavalli", "Nei","cGD")) )
		stop("Unrecognized mode")

	ret <- list(0)
	
	# distances among individuals
	if( mode=="Jaccard" ) ret <- .dist.jaccard(pop,loci)
	else if( mode=="Bray" ) ret <- .dist.bray(pop,loci)
	else if( mode=="AMOVA") ret <- .dist.amova(pop,loci)
	
	# population-level distances
	else {
		if( missing(stratum) )
			stop("Population-level genetic distances need the stratum parameter")
		
		if( mode == "Euclidean" ) ret <- .dist.euclidean( pop, stratum, loci )
		else if( mode== "Cavalli" ) ret <- .dist.cavalli( pop, stratum, loci )
		else if( mode== "Nei" ) ret <- .dist.nei( pop, stratum, loci )
		else if( mode== "cGD" ) ret <- .dist.cgd( pop, stratum, loci )
		else 
			stop("mode not recognized.")
			
		
	}
	
	
	ret
}





##############################################################################
#								HELPER FUNCTIONS                             #
##############################################################################




#' Jaccard dissimiarity 
#'
#' @param pop A \code{Population} object
#' @param loci The name of the locus or loci to use
#' @return distance matrix
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'
.dist.jaccard <- function( pop, loci) {
	N <- dim(pop)[1]
	ret <- list()
	
	cols <- indices(pop,loci)
	for( k in cols ){
		D <- matrix(0,nrow=N,ncol=N)
		for( i in 1:N ){
			p1 <- unique(pop[i,k]@alleles)
			if( length(p1) ) {
				for(j in (i+1):N ){
					if(j<=N ){
						p2 <- unique(pop[j,k]@alleles)
						if( length(p2) ) {
							aub <- length(union(p1,p2))
							anb <- length(intersect(p1,p2))
							if( aub )
								D[i,j] <- D[j,i] <- (aub-anb)/aub
						}
					}
				}
			}
			else 
				D[i,] <- D[j,] <- NA
		}
		locus <- names(pop)[k]
		ret[[locus]] <- D
	}
	return(ret)
}






#' Bray-Curtis dissimiarity 
#'
#' @param pop A \code{Population} object
#' @param loci The name of the locus or loci to use
#' @note  This function DOES replace missing data with the average genotype.
#' @return distance matrix
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'	
.dist.bray <- function( pop, loci ) {
	N <- dim(pop)[1]
	ret <- list()
	
	cols <- indices(pop,loci)
	for( k in cols ){
		all.freqs <- Frequencies( pop[,k] )
		alleles <- names(all.freqs)
		null.freqs <- get.frequencies(all.freqs)
		inds <- matrix( unlist( lapply( pop[,k],
						function(x) as.vector(x,alleles)))
						,byrow=TRUE, nrow=dim(pop)[1] )
		D <- matrix(0,nrow=N,ncol=N)
		for( i in 1:N ){
			p1 <- inds[i,]
			for(j in (i+1):N ){
				if(j<=N ){
					top <- sum(abs(p1-inds[j,]))
					bot <- sum(p1+inds[j,])
					D[i,j] <- D[j,i] <- top/bot
				}
			}
		}
		locus <- names(pop)[k]
		ret[[locus]] <- D
	}
	return(ret)
}




#' AMOVA distance
#'
#' @param pop A \code{Population} object
#' @param loci The name of the locus or loci to use
#' @return distance matrix
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'
.dist.amova <- function( pop, loci) {
	N <- dim(pop)[1]
	ret <- list()
	cols <- indices(pop,loci)
	D <- matrix(0,nrow=N,ncol=N)
	for( k in cols ){
		all.freqs <- Frequencies( pop[,k] )
		alleles <- names(all.freqs)
		null.freqs <- get.frequencies(all.freqs)
		inds <- matrix( unlist( lapply( pop[,k],
						function(x) as.vector(x,alleles)))
						,byrow=TRUE, nrow=dim(pop)[1] )
		inds[ rowSums(inds)==0, ] <- null.freqs
		
		for( i in 1:N ){
			p1 <- inds[i,]
			for(j in (i+1):N ){
				if(j<=N ){
					p2 <- inds[j,]
					d <- 0.5*((p1-p2) %*% (p1-p2) ) + D[i,j]
					D[i,j] <- D[j,i] <- d
				}
			}
		}

	}
	locus <- paste(loci,collapse=".")
	ret[[locus]] <- D
	return(ret)
}



#' Euclidean dissimiarity 
#'
#' @param pop A \code{Population} object
#' @param stratum The stratum of interest
#' @param loci The name of the locus or loci to use
#' @return distance matrix
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'
.dist.euclidean <- function( pop, stratum, loci) {
	pops <- partition(pop,stratum)
	K <- length(pops)
	D <- matrix(0,nrow=K,ncol=K)
	ret <- list()
	freqs <- lapply( pops, function(x) allele.frequencies( x, loci ) )
	cols <- indices(pop,loci)
	for(col in 1:length(cols) ) {
		for(i in 1:K) {
			f1 <- freqs[[i]][[col]]
			for( j in (i+1):K ){
				if( j <=K ) {
					f2 <- freqs[[j]][[col]]
					all.alleles <- union( names(f1),names(f2) )
					p1 <- get.frequencies(f1,all.alleles)
					p2 <- get.frequencies(f2,all.alleles)
					d <- sqrt( sum( (p1-p2)^2 ) ) + D[i,j]
					D[i,j] <- D[j,i] <- d
				}
			}
		}
	}
	
	locus <- paste(names(pop)[cols],collapse=".")
	ret[[locus]] <- D
	return(ret)
}




#' Cavalli-Sforza distance 
#'
#' @param pop A \code{Population} object
#' @param stratum The stratum of interest
#' @param loci The name of the locus or loci to use
#' @return distance matrix
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'
.dist.cavalli <- function( pop, stratum, loci) {
	pops <- partition(pop,stratum)
	K <- length(pops)
	ret <- list()
	freqs <- lapply( pops, function(x) allele.frequencies( x, loci ) )
	cols <- indices(pop,loci)
	for(col in 1:length(cols) ) {
		D <- matrix(0,nrow=K,ncol=K)
		for(i in 1:K) {
			f1 <- freqs[[i]][[col]]
			for( j in (i+1):K ){
				if( j <=K ) {
					f2 <- freqs[[j]][[col]]
					all.alleles <- union( names(f1),names(f2) )
					p1 <- get.frequencies(f1,all.alleles)
					p2 <- get.frequencies(f2,all.alleles)
					D[i,j] <- D[j,i] <- sqrt( 2-2*sum(sqrt(p1*p2)) )
				}
			}
		}
		locus <- loci[col]
		ret[[locus]] <- D
	}
	return(ret)
}





#' Nei dissimiarity 
#'
#' @param pop A \code{Population} object
#' @param stratum The stratum of interest
#' @param loci The name of the locus or loci to use
#' @return distance matrix
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'
.dist.nei <- function( pop, stratum, loci) {
	pops <- partition(pop,stratum)
	K <- length(pops)
	D <- matrix(0,nrow=K,ncol=K)
	ret <- list()
	freqs <- lapply( pops, function(x) allele.frequencies( x, loci ) )
	cols <- indices(pop,loci)
	for(i in 1:K) {
		f1 <- freqs[[i]]
		for( j in (i+1):K ){
			if( j <=K ) {
				f2 <- freqs[[j]]
				top <- 0
				bot1 <- 0
				bot2 <- 0
				for(col in 1:length(cols) ) {
					
					all.alleles <- union( names(f1[[col]]),names(f2[[col]]) )
					p1 <- get.frequencies(f1[[col]],all.alleles)
					p2 <- get.frequencies(f2[[col]],all.alleles)
					top <- top + sum(p1*p2)
					bot1 <- bot1 + sum(p1^2)
					bot2 <- bot2 + sum(p2^2)
				}
				D[i,j] <- D[j,i] <- -log( -1*log(top/sqrt(bot1*bot2)))
			}
		}
	}
	locus <- paste(names(pop)[cols],collapse=".")
	ret[[locus]] <- D
	return(ret)
}





#' cGD dissimiarity 
#'
#' @param pop A \code{Population} object
#' @param stratum The stratum of interest
#' @param loci The name of the locus or loci to use
#' @return distance matrix
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'
.dist.cgd <- function( pop, stratum, loci) {
	graph <- population.graph(pop,stratum)
	cols <- indices(pop,loci)
	locus <- paste(names(pop)[cols],collapse=".")
	ret <- list()
	ret[[locus]] <- shortest.paths(graph)
	return(ret)
}
