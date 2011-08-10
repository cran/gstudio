##############################################################################
# 							     genetic.stucture                            #
#                                                                            #
# 						    R.J. Dyer <rjdyer@@vcu.edu>                      #
#                                                                            #
##############################################################################

#' Main function for estimating genetic structure parameters
#'
#' @param pop A \code{\linkS4class{Population}} object
#' @param stratum The on which individuals will be partitioned
#' @param loci The locus or loci that the parameters will be estimated (see 
#'	note below).  If this option is not given, all loci will be used.
#' @param mode The parameter that is to be estimated.  Current options include
#' 	"Gst", "Gst.prime", and "Dest".
#' @param num.perm The number of permutations to used for significance testing.
#'	All significance is tested using permutation of individuals.
#' @param verbose A logical flag to indicate the locus progress (default=FALSE)
#' @return A \code{genetic.structure} object that has overloaded
#'	\code{print}, \code{summary}, and \code{plot} functions.
#' @seealso \code{\link{print.genetic.structure}}, 
#'	\code{\link{plot.genetic.structure}}
#' @note For multilocus estimates of these parameters, both individual single 
#'	locus and a multilocus parameter will be provied for \code{mode} types
#'	\eqn{G_{ST}}, \eqn{G_{ST}^\prime}, and \eqn{D_{est}}.  Of note, for 
#'	\eqn{G_{ST}}, \eqn{G_{ST}^\prime}, and \eqn{D_{est}} the numerator and
#'	denominantor of the multilocus parameter is accumulated across loci 
#'	before division.  You cannot just average single locus estimates or you
#'	can get a vastly different result (see REF). 
#' @references Joost, Hedrick, averaging gst one, 
#' @author Rodney J. Dyer <rjdyer@@vcu.edu
#' @export
genetic.structure <- function( 	pop=NULL,
								stratum=NULL,
								loci=NULL,
								mode=c("Gst","Gst.prime","Dest"),
								num.perm=0,
								verbose=FALSE
								)
{
	if( is.null(pop) || is.null(stratum) )
		stop("Both 'pop' and 'stratum' are required arguments for genetic.structure")
	if( is.null(loci) )
		loci <- column.names(pop,"Locus")
	if( !length(loci) )
		stop("You need to actually have some loci to estimate genetic structure.")
	if( !(mode %in% c("Gst","Gst.prime","Dest") ) || length(mode) > 1 )
		stop("Incorrect 'mode' option to genetic.structure.")

	#set up the return list
	ret <- list()
	ret$mode<-mode
	ret$stratum<-stratum
	ret$loci <- loci
	ret$estimate <- list()
	strata <- as.factor(as.character(pop[[stratum]]))
	strata.lvls <- levels(strata)
	k <- length(levels(strata))
	all.freqs <- allele.frequencies(pop,loci)
	all.alleles <- lapply(all.freqs, names)
	hs.mv <- 0
	ht.mv <- 0
	for( locus in loci ){
		if( verbose ) cat("Locus: ",locus)
		alleles <- all.alleles[[locus]]
		inds <- matrix( unlist( lapply(pop[[locus]],
					function(loc,alleles) 
						as.vector(loc,alleles), alleles=alleles)),
					ncol=length(alleles),byrow=T)
		#fixed locus
		if( dim(inds)[2]==1 ) {
			ret$estimate[[locus]] <- 0
			if( !missing(num.perm) && num.perm > 0 )
				ret$null.distribution[[locus]] <- 0
			
		} 
		else {
			p.vec <- colSums(inds)  # for cases where alleles are fixed
			ht <- 1 - sum((p.vec/sum(p.vec))^2)
			hs <- mean(unlist(lapply( strata.lvls, 
					function(strat,inds,strata ) {
						s <- colSums(inds[strata==strat,])
						f <- 1-sum((s/sum(s))^2)
					}, inds=inds, strata=strata)))
				
			n.harmonic <- 1/mean(1/table(strata))
			hs.estimated <- (2*n.harmonic)/(2*n.harmonic-1) * hs
			ht.estimated <- ht + hs.estimated/(2*k*n.harmonic)
		
			hs.mv <- hs.mv + hs.estimated
			ht.mv <- ht.mv + ht.estimated
		
			if( mode=="Gst" )
				ret$estimate[[locus]] <- 1-hs.estimated/ht.estimated
			else if( mode=="Gst.prime" ){
				gst.prime <- ((1-hs.estimated/ht.estimated)*(k-1+hs.estimated) )
				gst.prime <- gst.prime / ((k-1)*(1-hs.estimated))
				ret$estimate[[locus]] <-gst.prime
			}
			else if( mode == "Dest" ){
				D <- ((ht.estimated-hs.estimated) / (1-hs.estimated))
				D <- D / (k/(k-1))
				ret$estimate[[locus]] <- D
			}
		
			if( verbose ) cat("; ", mode,"=",ret$estimate[[locus]])
		
			if( !missing(num.perm) && num.perm > 0 ){
				perms <- rep(0,num.perm)
				for(i in seq(num.perm)) {
					hs.perm <- lapply( strata.lvls, 
									function(strat,inds,strata ) {
										s <- colSums(inds[strata==strat,]) 
										f <- 1 - sum((s/sum(s))^2) 
									}, 
									inds=inds, 
									strata=sample(strata))
					perms[i] <- mean(unlist(hs.perm))
				}

				perms <- (2*n.harmonic) / (2*n.harmonic-1) * perms

				if( mode=="Gst") 
					ret$null.distribution[[locus]] <- (1 - perms/ht.estimated)
				else if( mode=="Gst.prime") {
					gst.prime <- ((1-perms/ht.estimated)*(k-1+perms) )
					gst.prime <- gst.prime / ((k-1)*(1-perms))
					ret$null.distribution[[locus]] <- gst.prime
				}
				else if( mode=="Dest"){
					D <- ((ht.estimated-perms) / (1-perms))
					D <- D / (k/(k-1))
					ret$null.distribution[[locus]] <- D
				}
			
			
			
				if( verbose ) cat("; P =",(1+sum(ret$null.distribution[[locus]]>=ret$estimate[[locus]]))/(1+num.perm))
			}
		}
		if( verbose ) cat("\n")
	}
	
	if( length(loci)> 1  ){
		if( mode=="Gst" )
			ret$mv.estimate <- 1 - hs.mv/ht.mv
		else 
			ret$mv.estimate <- mean(unlist(ret$estimate))
	}
	
	class(ret) <- "genetic.structure"
	return(ret)
}


#' Prints out results of genetic structure
#'
#' Overload of print function for structure statistic
#' 
#' @param x A \code{genetic.structure} object
#' @param ... Ignored
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#'
print.genetic.structure <- function(x,...) {
	ret <- "Geneic Structure Analysis:\n"
	ret <- paste(ret, " Estimator:", x$mode,"\n" )
	ret <- paste(ret, " Stratum:", x$stratum, "\n")
	ret <- paste(ret, " Loci: {",paste(x$loci,collapse=", "), "}\n")
	
	for( i in 1:length(x$loci) ) {
		locus <- x$loci[i]
		ret <- paste( ret, "  -",locus,"; ",x$mode,"=",x$estimate[[locus]])
		if( "null.distribution" %in% names(x) ){
			P <- 1+sum( x$null.distribution[[locus]] >= x$estimate[[locus]]   )
			P <- P / (1+length(x$null.distribution[[locus]]))
			ret <- paste( ret, "; P =",P)
		}
		ret <- paste(ret,"\n")
	}	
	if( "mv.estimate" %in% names(x) )
		ret <- paste(ret," MV:",x$mv.estimate,"\n")
	cat(ret) 
	invisible(x)
}

#' Plots out results of genetic structure
#'
#' Overload of plot function for structure statistic
#' 
#' @param x A \code{genetic.structure} object
#' @param y Ignored
#' @param ... Ignored
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#'
plot.genetic.structure <- function(x,y,...){
	if( !("null.distribution" %in% names(x) )) 
		stop("To plot a genetic.structure object, you need to test significance")
	
	for(locus in x$loci ) {
		cat(locus,"\n")
		null.dist <- c(x$null.distribution[[locus]],x$estimate[[locus]])

		P <- sum( null.dist >= x$estimate[[locus]]   )
		P <- P / (1+length(x$null.distribution[[locus]]))
		
		
		h <- hist( null.dist, freq=T,xlab=paste("Permuted",x$mode), ylab="Frequency",bty="n",main=paste(x$mode,"for",locus))
		x.obs <- rep(x$estimate[[locus]],2)
		y.obs <- c( 0, max(h$counts)*.95 )
		par(new=T)
		plot( x.obs, y.obs, xlim=range(h$breaks), ylim=c(0,max(h$counts) ), col="red", lwd=2, pch=c(-1,1),type="b",bty="n" ,axes=F,xlab="",ylab="")
		text(x.obs[1],max(h$counts),"obs",col="red")
		mtext(paste("P",P,sep="="))
		
		if( length(x$loci)>1) 
			readline("Hit {Return} to See next plot:")
	}
	cat("\n")
	
}




