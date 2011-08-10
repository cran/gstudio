##############################################################################
# 							      Locus Class                                #
#                                                                            #
# 						    R.J. Dyer <rjdyer@@vcu.edu>                      #
#                                                                            #
##############################################################################





##############################################################################
#								CLASS DEFINITION                             #
##############################################################################



#' Class \code{"Locus"}
#' 
#' A description of the \code{Locus} class
#' 
#' 
#' @name Locus-class
#' @docType class
#' @note Notes on \code{Locus}
#' @author Rodney J. Dyer <rjdyer@@vcu.edu
#' @references Locus references
#' @keywords classes
#' @exportClass Locus
#'
setClass("Locus",
         representation( alleles="character" ),
         prototype( alleles=character(0) )
)





##############################################################################
#								HELPER FUNCTIONS                             #
##############################################################################



#' Constructor function for \code{\linkS4class{Locus}} objects
#' 
#' Locus class description
#' 
#' This function will not sort the alleles but will keep them in normal order
#' 
#' @param alleles A vector of objects that can be turned into \code{character}
#' @return A fully formed \code{\linkS4class{Locus}} object.
#' @note Missing data can be encoded as either '-9' or 'NA'.  If there is a 
#'	single missing allele at the locus, then the entire locus will be 
#' 	considered as missing.  If you are mixing diploid and haploid individuals,
#'	encode the haploids as only a single allele and do not try to put in the 
#'	extra allele as missing.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @seealso \code{\linkS4class{Locus}}
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' 
#' loc <- Locus( 1:3 )
#' loc <- Locus( c("A","T"))
#'

Locus <- function( alleles=character(0) ) {
	if( missing(alleles)) alleles <- character(0)
	else if( !length(alleles)) alleles <- character(0)
	else if( "-9" %in% alleles ) 
		warning("Don't use negative numbers to encode missing data, use NA")
	else if( "NA" %in% alleles ) alleles <- character(0)
	else if( "na" %in% alleles ) alleles <- character(0)
	if( class(alleles) != "character") alleles <- as.character(alleles) 
	new("Locus",alleles=alleles )
}





##############################################################################
#							OVERRIDING GENERIC FUNCTIONS                     #
##############################################################################



#' Printing a locus object
#' 
#' This overloads the print function and prints the Locus
#' 
#' 
#' @param x A \code{Locus} object
#' @return A character representation of the \code{Locus}
#' @docType methods
#' @aliases print,Locus-method
#' @rdname print
#' @exportMethod print
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loc <- Locus( c("A","T") )
#' print(loc)
#'
setMethod("print",
    signature(x = "Locus"),
    function (x, ...) 
    {
        return(paste(x@alleles,collapse=":"))
    }
)

#' Showing a locus object
#' 
#' This overloads the show function and prints the Locus
#' 
#' 
#' @param x A \code{Locus} object
#' @param ... Ignored
#' @docType methods
#' @aliases show,Locus-method
#' @rdname show
#' @exportMethod show
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loc <- Locus( c("A","T") )
#' loc
#'
setMethod("show",
    "Locus",
    function (object) 
    {
		if( is.na(object) )
			cat(NA,"\n")
		else
        	cat(paste(object@alleles,collapse=":"),"\n")
    }
)





#' Accessing elements within a locus object
#' 
#' This is an overload for [] indexing of alleles
#' 
#' 
#' @name [-Locus-method
#' @aliases [,Locus-method
#' @docType methods
#' @rdname Locus-accessors
#' @exportMethod "["
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loc <- Locus( c("A","T") )
#' loc[1]
#' 
setMethod("[",
    signature(x = "Locus"),
    function (x, i, j, ..., drop = TRUE) 
    {
        return(x@alleles[i])
    }
)



#' Accessing elements within a locus object
#' 
#' This is an overload for []<- indexing of alleles
#' 
#' 
#' @name [<--Locus-method
#' @docType methods
#' @aliases [<-,Locus-method
#' @rdname Locus-accessors
#' @exportMethod "[<-"
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loc <- Locus( c("A","T") )
#' loc[1] <- "C"
#'
setReplaceMethod("[",
    signature(x = "Locus"),
    function (x, i, j, ..., value) 
    {
		x@alleles[i] <- as.character(value)
		x
    }
)



#' Overload of \code{as.character} function
#' 
#' Returns character version of the \code{Locus}
#' 
#' 
#' @param x A \code{Locus} object
#' @param ... Ignored
#' @return A character version of the \code{Locus}
#' @docType methods
#' @aliases as.character,Locus-method
#' @rdname as.character
#' @exportMethod as.character
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loc <- Locus( c("A","T") )
#' as.character(loc)
#'
setMethod("as.character",
    signature(x = "Locus"),
    function (x, ...) 
    {
		if(!length(x))
			return(NA)
		else
        	return(paste(x@alleles,collapse=":"))
    }
)



#' Overload of \code{as.vector}
#' 
#' This function is an overload of the \code{as.vector} function and returns 
#'	the locus as a multivariate vector whose alleles are indexed by the vector 
#' 	of alleles you pass via \code{mode}
#' 
#' @param x A \code{Locus} object
#' @param mode A vector of allele states
#' @return A multivariate version of the \code{Locus}
#' @docType methods
#' @aliases as.vector,Locus-method
#' @rdname as.vector
#' @exportMethod as.vector
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loc <- Locus( c("A","T") )
#' as.character(loc)
#'
setMethod("as.vector",
	signature(x = "Locus"),
	function (x, mode = "ANY") 
	{
		if(!is(mode,"character"))
			mode <- as.character(mode)
		ret <- rep(0,length(mode))
		for(allele in x@alleles ) {
			idx <- which(allele == mode)
			if( length(idx) )
				ret[idx] <- ret[idx] + 1
		}
		ret
	}
)



#' Overload of \code{is.na}
#' 
#' This function is an overload of the \code{is.na} function and returns a 
#'	logical indicating if the locus is empty.
#' 
#' @param x A \code{Locus} object
#' @return A logical indicating that the locus is empty
#' @docType methods
#' @aliases is.na,Locus-method
#' @rdname is.na
#' @exportMethod is.na
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loc1 <- Locus( c("A","T") )
#' is.na(loc1)
#' loc2 <- Locus( )
#' is.na(loc2)
#'
setMethod("is.na",
    signature(x = "Locus"),
    function (x) 
    {
        return( length(x@alleles) == 0 )
    }
)



#' Overload of \code{length}
#' 
#' This function is an overload of the \code{length} function and returns the 
#'	number of alleles at the locus (e.g., the ploidy).
#' 
#' @param x A \code{Locus} object
#' @return The number of alleles
#' @docType methods
#' @aliases length,Locus-method
#' @rdname length
#' @exportMethod length
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loc1 <- Locus( c("A","T") )
#' length(loc1)
#' loc2 <- Locus( 1:4 )
#' length(loc2)
#'
setMethod("length",
    signature(x = "Locus"),
    function (x) 
    {
		return(length(x@alleles))
    }
)


#' Overload of \code{summary}
#' 
#' This function is an overload of the \code{summary} function and returns the 
#'	a rather boring summary of the locus
#' 
#' @param object A \code{Locus} object
#' @return A summary.Locus class
#' @docType methods
#' @aliases summary,Locus-method
#' @rdname summary
#' @exportMethod summary
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loc1 <- Locus( c("A","T") )
#' summary(loc1)
#'
setMethod("summary",
    signature(object = "Locus"),
    function (object, ...) 
    {
        ret <- list(Class="Locus")
		ret["Ploidy"] <- length(object)
		alleles <- paste(as.character(sort(unique(object@alleles))), collapse=",")
		ret["Aleleles"] <- alleles
		class(ret) <- "summary.gstudio"
		ret
    }
)







##############################################################################
#								NEW GENERIC FUNCTIONS                        #
##############################################################################



#' Is a locus a heterozygote
#' 
#' @param locus A \code{Locus} object
#' @docType methods
#' @aliases is.heterozygote
#' @rdname is.heterozygote_Locus
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
setGeneric("is.heterozygote",
	def=function(locus) standardGeneric("is.heterozygote") )


#' @rdname is.heterozygote_Locus
#' @aliases is.heterozygote,Locus-method
#' @exportMethod is.heterozygote
#' @examples \dontrun{
#'
#' loc <- Locus( c("A","T") )
#' is.heterozygote(loc)
#'  
#' }
setMethod("is.heterozygote",
	signature(locus="Locus"),
	function(locus) {
		if( length(locus@alleles) < 2 )
			return(FALSE)
		a <- locus@alleles[1]
		return( any( locus@alleles != a ) )
	}
)






