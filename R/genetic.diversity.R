#' Estimates rarefied genetic diversity
#'
#' Uses rarefaction to estimate measures of genetic diversity that are
#'	standardized by sample size.
#' @param pop A \code{Population} object
#' @param stratum The stratum to use
#' @param loci The genetic locus to use.  
#' @param mode The kind of diversity to estimate. Available types are 
#'	A = the number of alleles, A95 = the number of alleles whose 
#'	frequencies are at least 5 percent, and Ae=inverse of expected
#'	homozygosity.
#' @param num.perm The number of permutations to use for rarefaction
#' @param verbose A logical flag to indicate if the progress of the 
#'	function should be portrayed on screen.
#' @return A genetic.diversity object.
#' @export
#'
genetic.diversity <- function(	pop,
								stratum,
								loci,
								mode=c("A","A95","Ae")[3],
								num.perm=999,
								verbose=FALSE ) 
{
	if(missing(pop) || missing(stratum)  )
		stop("pop and stratum are both required for genetic.diversity.")
	if( !(stratum %in% names(pop)) )
		stop("the stratum passed is not a data type of the pop.")
	if( missing(loci) )
		loci <- column.names(pop,"Locus")
	if( !(mode %in% c("A","A95","Ae")))
		stop("Unrecognized diversity mode")
	
	strata <- as.factor(pop[[stratum]])
	popnames <- as.character(levels(strata))
	pops <- partition(pop,stratum)

	K <- length(levels(strata))
	L <- length(loci)
	
	params <- matrix(0,nrow=L,ncol=K)
	row.names(params) <- loci
	colnames(params) <- popnames
	
	
	rarefaction <- list()
	
	for( i in 1:L ) {
		locus <- loci[i]	
		
		if(verbose) cat("Locus",locus,":")

		freqs <- lapply(pops,function(x) allele.frequencies(x,locus))
		
		for(j in 1:K){
			cts <- freqs[[j]][[locus]]
			f <- get.frequencies(cts)
			if( mode == "A")
				params[i,j] <- sum( f > 0 )
			else if( mode == "A95" ) 
				params[i,j] <- sum( f >= 0.05)	
			else if( mode == "Ae" )
				params[i,j] <- 1 / sum(f^2)
		}
		
		# do the rarefaction
		if(num.perm > 0 ) {		
			counts <- lapply( freqs, function(x) sum(unlist(x[[1]]@counts)))
			minSz <- min( unlist(counts) )
			p <- matrix(0,nrow=num.perm,ncol=K)	
			colnames(p) <- popnames
			for(j in 1:K) {
				
 				
				cts <- unlist(freqs[[j]][[locus]]@counts)
				alleles <- rep(names(cts), cts )
				for(k in 1:num.perm){
					if(verbose && !(k %% 100)) cat(".")
					
					samp <- table( sample(alleles,minSz,replace=T)  )
					if(mode=="A")
						p[k,j] <- length(samp)
					else if( mode=="A95") 
						p[k,j] <- sum( (samp/sum(samp)) > 0.05 )
					else if( mode=="Ae" ) {
						f <- (samp/sum(samp))
						p[k,j] <- 1/sum(f^2)
					}
				}
			}
			rarefaction[[locus]] <- p
		}
		
		if(verbose) cat("\n")
	
	}
	
	# create the return object
	ret <- list()
	ret$mode <- mode
	ret$stratum <- stratum
	ret$loci <- loci
	ret$estimate <- params
	ret$perms <- rarefaction
	class(ret) <- "genetic.diversity"
	return(ret)
}





#' Prints out results of genetic diversity
#'
#' Overload of print function for genetic.diversity statistic
#' 
#' @param x A \code{genetic.diversity} object
#' @param ... Ignored
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#'
print.genetic.diversity <- function(x,...) {
	ret <- "Geneic Diversity:\n"
	ret <- paste(ret, " Estimator:", x$mode,"\n" )
	ret <- paste(ret, " Stratum:", x$stratum, "\n")
	ret <- paste(ret, " Loci: {",paste(x$loci,collapse=", "), "}\n")
	
	obs <- x$estimate
	nLoci <- dim(obs)[1]
	nPops <- dim(obs)[2]
	popnames <- colnames(obs)
	
	for( i in 1:nLoci ) {
		locus <- x$loci[i]
		ret <- paste( ret, " Locus =",locus,"\n")
		for( j in 1:nPops ) {
			ret <- paste(ret, "  ",popnames[j],x$mode,"=",obs[i,j])
			if( length(x$perms[[locus]])){
				r <- mean(x$perms[[locus]][,j])
				ret <- paste(ret,"; Rarefaction",x$mode,"=",r) 
			}
			ret <- paste(ret,"\n")
		}

	}	
	cat(ret) 
	invisible(x)
}



#' Plots out results of genetic diversity
#'
#' Overload of plot function for genetic.diversity 
#' 
#' @param x A \code{genetic.diversity} object
#' @param y The name of the locus to plot, if this is missing then all loci will 
#'	be examined and you will be prompted to cycle through them.
#' @param ... Ignored
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#'
plot.genetic.diversity <- function(x,y,...){
	if( !("perms" %in% names(x) )) 
		stop("To plot a genetic.diversity object, you need to do some permutations")
		
	if( missing(y) )
		loci <- x$loci
	else if( y %in% x$loci)
		loci <- y
	else
		stop("The locus requested to plot is not in the diversity results")
	
	for(locus in loci ) {
		vals <- x$perms[[locus]]
		
		boxplot(vals,horizontal=T,
				notch=T,
				xlab=x$mode, ylab=x$stratum,
				main=paste("Genetic Diversity",locus) )
		
		
		if( length(loci) > 1 && locus != loci[length(loci)] )
			readline("Hit {Return} to See next plot:")
	}
}






