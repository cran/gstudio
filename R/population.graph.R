##############################################################################
# 							     population.graph                            #
#                                                                            #
# 						    R.J. Dyer <rjdyer@@vcu.edu>                      #
#                                                                            #
##############################################################################

#' Create a Population Graph from a data set
#'
#' Takes a population and creates a population graph topology in terms of an
#' adjacency matrix.
#'
#' @param pop A \code{Population} object
#' @param stratum The stratum that you want to represent the node set
#' @param loci The subset of loci to use. If this is not given then all loci
#'	in the population will be used (the default).
#' @param alpha The alpha level at which you want the edges to be tested.
#' @return An igraph object of the population graph with node names and sizes
#'	as vertex attributes.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#'
population.graph <- function( 	pop,
								stratum=NULL,
								loci=NULL,
								alpha=0.05 ) {
	if(!require(MASS))
		stop("This function needs the MASS library, please install it")
	if(!require(igraph))
		stop("This function needs the igraph libary, please install it")
	
	if( !is(pop,"Population") )
		stop("Need a population ")
	if( missing(stratum) || !(stratum %in% names(pop)) )
		stop("You need to specify a stratum to partition your data on.")
	if( missing(loci) || is.null(loci) )
		loci <- column.names(pop,"Locus")
	cat("tranforming data... ")
	genos <- multivariate.loci( pop, loci, ploidy=2 )
	cat("done\n")
	pops <- as.factor( pop[[stratum]] )
	N <- length(pops)
	npop <- length(levels(pops))
	R <- matrix(1,npop,npop)
	D <- matrix(0.0,nrow=npop,ncol=npop)
	critVal <- qchisq( 1.0-alpha,1 )
	SRI <- matrix(1,npop,npop)
	EED <- matrix(0,npop,npop)
	EdgeStr <- matrix(0,npop,npop)
	cat("Rotating mv genos and partitioning... ")
	pcfit <- prcomp(genos,retx=T)
	genos <- as.matrix(pcfit$x[,pcfit$sdev > 1.0e-3])
	fit1 <- lda(x=genos,grouping=pops,method="moment")
	cat(" done\n")
	means <- colMeans(fit1$means)
	LDValues <- scale( genos, center=means, scale=FALSE ) %*% fit1$scaling
	allLD <- by(LDValues,pops,mean)
	allSD <-  by(LDValues,pops,sd)
	
	for(i in seq(1,npop)) 
		for(j in seq(1,npop)){
			p1 <- unlist( allLD[i])
			p2 <- unlist( allLD[j])
			D[i,j] = sqrt( sum( (p1-p2)^2 ))
		}
	totMean <- mean(D)
	colMean <- colMeans(D)
	totMeanMatrix = matrix(1,npop,npop)*totMean
	colMeanMatrix = matrix(colMean,npop,npop,byrow=T)
	rowMeanMatrix = matrix(colMean,npop,npop,byrow=F)
	cat("Estimating conditional genetic covariance... ")
	C <- -0.5 * (D - colMeanMatrix - rowMeanMatrix + totMeanMatrix)
	for( i in 1:npop ) for( j in 1:npop ) if(i!=j)
		R[i,j] = C[i,j]/sqrt( C[i,i] * C[j,j] )
	RI <- ginv(R)
	for(i in seq(1,npop)) for(j in seq(1,npop)) if(i!=j) 
		SRI[i,j] <- -1*RI[i,j]/sqrt( RI[i,i]*RI[j,j])
	for(i in seq(1,npop)) for(j in seq(1,npop)) if(i!=j) {
		EED[i,j] = -N * log(1-SRI[i,j]^2)
		EdgeStr[i,j] = -0.5 * log(1-SRI[i,j]^2)
	}	
	D[ EED<=critVal ] <- 0
	cat("done\n")

	ret <- graph.adjacency(D,mode="undirected",weight=TRUE,diag=FALSE)
	V(ret)$name <- names(allSD)
	popSD <- unlist(lapply(allSD,function(x) sum(unlist(x)) ))
	popSD <- popSD-min(popSD)
	popSD <- popSD/max(popSD) * 15 + 5
	V(ret)$size <- popSD
	
 	return(ret)
};






