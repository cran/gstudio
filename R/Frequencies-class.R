##############################################################################
#                                                                            #
# 							     Frequencies Class                           #
#                                                                            #
# 						    R.J. Dyer <rjdyer@@vcu.edu>                      #
#                                                                            #
##############################################################################


##############################################################################
#                                                                            #
#								CLASS DEFINITION                             #
##############################################################################



#' Class \code{"Frequencies"}
#' 
#' The \code{Frequencies} class is the base class that estimates allele
#' 	frequencies for a single locus.  It can also provide estimates of observed
#' 	and expected heterozygosity. 
#' 
#' @name Frequencies-class
#' @docType class
#' @note Notes on \code{Frequencies}
#' @author Rodney J. Dyer <rjdyer@@vcu.edu
#' @keywords classes
#' @exportClass Frequencies
#'
setClass("Frequencies",
         representation( counts="list",
 						 nHet="numeric",
						 nDip="numeric",
						 n="numeric"),
         prototype( counts=list(), nHet=0, nDip=0,n=0 )
)





##############################################################################
#                                                                            #
#								HELPER FUNCTIONS                             #
##############################################################################



#' Constructor function for \code{\linkS4class{Frequencies}} objects
#' 
#' This function will not sort the alleles but will keep them in normal order
#' 
#' @param loci A list of \code{Locus} objects.  Can be empty
#' @return A fully formed \code{\linkS4class{Frequencies}} object.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @seealso \code{\linkS4class{Locus}}, \code{\linkS4class{Population}}
#' @export
#' @examples
#' 
#' loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) ) 
#' freqs <- Frequencies( loci )
#' freqs
#'

Frequencies <- function( loci=list() ) {
	if( missing(loci) || !length(loci) )
		stop("Frequencies must be constructed with loci...")
	ret <- new("Frequencies")
	for(locus in loci){
		ret@n <- ret@n+1
		if( !is(locus,"Locus"))
			stop("Only pass 'Locus' objects to Frequencies()")
	
		for(a in locus@alleles) {
			if( !is.na(a) ) {
				if(!(a %in% names(ret@counts) )) 
					ret@counts[[a]]<-1
				else
					ret@counts[[a]] <- ret@counts[[a]]+1
			}
		}
		if( length(locus)>1 ) {
			ret@nDip <- ret@nDip+1
			if( is.heterozygote(locus) )
				ret@nHet <- ret@nHet+1
		}
		
	}
	ret
}





##############################################################################
#                                                                            #
#							OVERRIDING GENERIC FUNCTIONS                     #
##############################################################################



#' Printing a \code{Frequency} object
#' 
#' This overloads the print function and prints the \code{Frequencies}
#' 
#' @param x A \code{Frquencies} object
#' @return A character representation of the \code{Frequencies} object
#' @docType methods
#' @aliases print,Frequencies-method
#' @rdname print
#' @exportMethod print
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) ) 
#' freqs <- Frequencies( loci )
#' print(freqs)
#'
setMethod("print",
    signature(x = "Frequencies"),
    function (x, ...) {
		f <- get.frequencies(x)
		return(as.table(f))
    }
)

#' Showing a Frequencies object
#' 
#' This overloads the show function and prints the \code{Frequencies}
#' 
#' 
#' @param x A \code{Frequencies} object
#' @docType methods
#' @aliases show,Frequencies-method
#' @rdname show
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @exportMethod show
#' @examples
#'
#' loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) ) 
#' freqs <- Frequencies( loci )
#' freqs  ## equivalent to show(freqs)
#'
setMethod("show",
    "Frequencies",
    function (object) 
    {
		f <- get.frequencies(object)
		a <- names(f)
		cat("Allele Frequencies:\n")
		for(i in 1:length(f))
			cat(" ",a[i],"=",f[i],"\n")
		invisible(object)
    }
)




#' Overload of \code{summary} for \code{Frequencies} object
#' 
#' This function is an overload of the \code{summary} function for the
#'	\code{Frequencies} object.
#' 
#' @param object A \code{Frequencies} object
#' @return A summary.Frequencies class
#' @docType methods
#' @aliases summary,Frequencies-method
#' @rdname summary
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @exportMethod summary
#' @examples
#'
#' loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) ) 
#' freqs <- Frequencies( loci )
#' summary(freqs)
#'
setMethod("summary",
    signature(object = "Frequencies"),
    function (object, ...) {
        ret <- list(Class="Frequencies")
		ret$N <- object@n
		ret$A <- paste("{",paste(as.character(sort(names(object@counts))), collapse=", "),"}")
		ret$ho <- ho(object)
		ret$he <- he(object)
		class(ret) <- "summary.gstudio"
		ret
    }
)


#' Get number of alleles in \code{Frequency} object
#' 
#' This overloads the length function for \code{Frequencies}.  It returns the
#'	number of alleles in the object.
#' 
#' @param x A \code{Frquencies} object
#' @return The number of alleles observed
#' @docType methods
#' @aliases length,Frequencies-method
#' @rdname length
#' @exportMethod length
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) ) 
#' freqs <- Frequencies( loci )
#' length(freqs)
#'
setMethod("length",
    signature(x = "Frequencies"),
    function (x) {
		return(length(x@counts))
    }
)


#' Get alleles in \code{Frequency} object
#' 
#' This overloads the names function and returns the alleles in the \code{Frequencies} 
#'	object
#' 
#' @param x A \code{Frquencies} object
#' @return A character list of alleles
#' @docType methods
#' @aliases names,Frequencies-method
#' @rdname names
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @exportMethod names
#' @examples
#'
#' loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) ) 
#' freqs <- Frequencies( loci )
#' names(freqs)
#'
setMethod("names",
    signature(x = "Frequencies"),
    function (x) {
		return(names(x@counts))
    }
)






##############################################################################
#                                                                            #
#								NEW GENERIC FUNCTIONS                        #
##############################################################################



#' Gets allele frequencies from pop
#' 
#' @param freqs A \code{Frequencies} object
#' @param alleles A list of alleles
#' @return A vector of allele frequencies
#' @docType methods
#' @aliases get.frequencies
#' @rdname get.frequencies_Frequencies
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
setGeneric("get.frequencies",
	def=function(freqs, alleles) standardGeneric("get.frequencies") )


#' @rdname get.frequencies_Frequencies
#' @aliases get.frequencies,Frequencies-method
#' @exportMethod get.frequencies
#' @examples \dontrun{
#'
#' loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) ) 
#' freqs <- Frequencies( loci )
#' get.frequencies( freqs, c(1,2,3) )
#'
#' }
setMethod("get.frequencies",
	signature(freqs="Frequencies"),
	function(freqs,alleles) {
		if( missing(alleles) )
			alleles <- names(freqs@counts)
		if( !is(alleles,"character") )
			alleles <- as.character(alleles)
		totCt <- sum(unlist(freqs@counts))
		k <- length(alleles)
		ret <- rep(0,k)
		if( totCt )
			for(i in 1:k) 
				if( alleles[i] %in% names(freqs@counts) )
					ret[i] <- freqs@counts[[alleles[i]]] / totCt
		names(ret) <- alleles
		return(ret)
	}
)





#' Gets observed heterozygosity
#' 
#' @param freqs A \code{Frequencies} object
#' @param size.corrected A logical flag to indicate that you want to correct
#' 	the estimation of observed heterozygosity by the number of loci sampled
#' @return An estimate of observed heterozygosity
#' @docType methods
#' @aliases ho
#' @rdname ho_Frequencies
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
setGeneric("ho",
	def=function(freqs, size.corrected) standardGeneric("ho") )


#' @rdname ho_Frequencies
#' @aliases ho,Frequencies-method
#' @exportMethod ho
#' @examples \dontrun{
#'
#' loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) ) 
#' freqs <- Frequencies( loci )
#' ho( freqs, size.corrected=FALSE )
#' ho( freqs, size.corrected=TRUE )
#'
#' }
setMethod("ho",
	signature(freqs="Frequencies"),
	function(freqs,size.corrected) {
		h <- 0
		if(freqs@nDip) {
			h <- freqs@nHet/freqs@nDip
			names(h) <- "ho"
			if( !missing(size.corrected) && size.corrected ){
				h <- h * (2*freqs@nDip)/(2*freqs@nDip-1)
				names(h) <- "ho.cor"
			}
		}
		return(h)
	}
)


#' Gets expected heterozygosity
#' 
#' @param freqs A \code{Frequencies} object
#' @param size.corrected A logical flag to indicate that you want to correct the estimation
#'	of observed heterozygosity by the number of loci sampled
#' @return An estimate of expected heterozygosity
#' @docType methods
#' @aliases he
#' @rdname he_Frequencies
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
setGeneric("he",
	def=function(freqs, size.corrected) standardGeneric("he") )


#' @rdname he_Frequencies
#' @aliases he,Frequencies-method
#' @exportMethod he
#' @examples \dontrun{
#'
#' loci <- list( Locus(c(1,1)), Locus(c(1,1)), Locus(c(1,2)), Locus(c(2,2)) ) 
#' freqs <- Frequencies( loci )
#' he( freqs, size.corrected=FALSE )
#' he( freqs, size.corrected=TRUE )
#'
#' }
setMethod("he",
	signature(freqs="Frequencies"),
	function(freqs,size.corrected) {
		f <- get.frequencies(freqs,names(freqs@counts))
		h <- 1 - sum( f^2 )
		names(h) <- "he"
		if( !missing(size.corrected) && size.corrected) {
			h <- h * (2*freqs@nDip)/(2*freqs@nDip-1)
			names(h) <- "he.cor"
		}
		return(h)
	}
)











