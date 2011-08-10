##############################################################################
# 							    Population Class                             #
#                                                                            #
# 						    R.J. Dyer <rjdyer@@vcu.edu>                      #
#                                                                            #
##############################################################################



##############################################################################
#								CLASS DEFINITION                             #
##############################################################################



#' Class \code{"Population"}
#' 
#' A description of the \code{Population} class
#' 
#' 
#' @name Population-class
#' @docType class
#' @note Notes on \code{Population}
#' @author Rodney J. Dyer <rjdyer@@vcu.edu
#' @keywords classes
#' @exportClass Population
#'
setClass("Population",
	representation(	values="list"),
	prototype( values=list() )
)







##############################################################################
#								HELPER FUNCTIONS                             #
##############################################################################



#' Constructor function for \code{\linkS4class{Population}} objects
#' 
#' Locus class description
#' 
#' This function will not sort the alleles but will keep them in normal order
#' 
#' @param ... A variable number of vectors and lists that can be passed to the 
#' 	Population. All parameters have to be named (e.g. x=rnorm(20), etc) and 
#'	cannot be anonymous
#' @return A fully formed \code{\linkS4class{Population}} object.
#' @note This is a general data.frame-like object that can easily hold strata, 
#'	covariates, and \code{\linkS4class{Locus}} objects.  You can append data 
#'	columns onto this after it has been constructed but you cannot append rows 
#'	onto it.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @seealso \code{\linkS4class{Locus}}
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'  
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(1,1)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' pop
#'
Population <- function(...){
	pop <- new("Population")
	nRows <- -1
	args <- list(...)
	if( length(args) ){		
		if(!length(names(args)))
			stop("You need to provide named arguments for Population() (see docs).")

		for(name in names(args)) {
			lst <- args[[name]]
			if( nRows == -1 ) nRows = length(lst)
			else if( nRows != length(lst) )
				stop("All list lengths must be the same to make a population.")
			pop@values[[name]] <- lst
		}	
	}	
	return(pop)
}






##############################################################################
#							OVERRIDING GENERIC FUNCTIONS                     #
##############################################################################


#' Data column names in \code{Population} a locus object
#' 
#' This overloads the names function and prints the data column names
#' 
#' @param x A \code{Population} object
#' @docType methods
#' @return A vector of column names or NULL if empty
#' @aliases names names,Population-method
#' @rdname names
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(1,1)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' names(pop)
#'
setMethod("names",
	signature("Population"),
	function(x) {
		return(names(x@values))
	}
)

#' Dimensions of the \code{Population}
#' 
#' A length two vector of row and column sizes of the \code{Population}
#' 
#' @param x A \code{Population} object
#' @return A two element vector with the number of rows and columns
#' @docType methods
#' @aliases dim,Population-method
#' @rdname dim
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(1,1)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' dim(pop)
#'
setMethod("dim",
	signature("Population"),
	function(x) {
		cols <- names( x@values )
		if( is.null(cols) ) 
			return(c(0,0)) 
		ncols <- length(cols)
		nrows <- 0
		if( length(ncols))
			nrows <- length( x@values[[cols[1]]] )
		ret <- c(nrows,ncols)
		ret 
	}
)



#' Number of data columns
#' 
#' The length of a \code{Population} is defined as the number of data columns
#' 
#' @param x A \code{Population} object
#' @return The number of data columns
#' @docType methods
#' @aliases length,Population-method
#' @rdname length
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(1,1)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' length(pop)
#'
setMethod("length",
	signature("Population"),
	function(x) {
		return( length(names(x)))
	}
)


#' Overload Show function for \code{Population}
#' 
#' Prints out the data within the \code{Population}
#' 
#' @param x A \code{Population} object
#' @docType methods
#' @aliases show,Population-method
#' @rdname show
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(1,1)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' pop  ## equivalent to show(pop)
#'
setMethod("show",
	signature("Population"),
	function(object) {
		if( length(object) == 0 )
			cat("Populations with no individuals are not very interesting...\n")
		else {
			df <- data.frame()
			if( length(object@values) ) {
				nCols <- length(object)
				nRows <- length(object@values[[1]])
				df <- data.frame(row.names=as.character(1:nRows))
				for( i in 1:nCols ){
					vals <- object@values[[i]]
					if( is(vals[[1]],"Locus")) 
						for(j in 1:nRows)
							vals[[j]] <- as.character(vals[[j]])
					df[,i] <- unlist(vals)
				}
				names(df) <- names(object)
			}
			show(df)
		}
		invisible(object)
	}
)



#' Overload Summary function for \code{Population}
#' 
#' Prints out a summary of the \code{Population}
#' 
#' @param object A \code{Population} object
#' @docType methods
#' @aliases summary,Population-method
#' @rdname summary
#' @examples
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' summary(pop)
#'
setMethod("summary",
	signature("Population"),
	function(object,...) {
		if( length(object) == 0 )
			cat("Populations with no individuals are not very interesting...\n")
		else {
			df <- data.frame()
			if( length(object@values) ) {
				nCols <- length(object)
				nRows <- length(object@values[[1]])
				df <- data.frame(row.names=as.character(1:nRows))
				for( i in 1:nCols ){
					vals <- object@values[[i]]
					if( is(vals,"list")) {
						for(j in 1:nRows)
							vals[[j]] <- as.character(vals[[j]])
						vals <- as.factor(unlist(vals))
					}
					df[,i] <- unlist(vals)
				}
				names(df) <- names(object)
			}
			return(summary(df))
		}
		invisible(object)
	}
)








#' Overload row.names function for \code{Population}
#' 
#' Returns a list of row names for the \code{Population}
#' 
#' @param x A \code{Population} object
#' @return Row names for the population
#' @note  This is kind of a stupid function in its present form, there are no 
#'	specific row names in a \code{Population} object, you only index rows by 
#'	number.  So this just returns a 1:nRows numeric array...  
#' @docType methods
#' @aliases row.names,Population-method
#' @rdname row.names
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' row.names(pop)
#'
setMethod("row.names",
	signature("Population"),
	function(x){
		if( length(x) )
			return(1:length(x[,1]))
		else
			return(NULL)
	}
		
)





#' Accessing data column within a Population object
#' 
#' This is an overload for $ indexing data columns in a \code{Population}
#' 
#' @name $
#' @aliases $,Population-method
#' @docType methods
#' @rdname Population-accessors
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' pop$env
#'
setMethod("$",
	signature("Population"),
	function(x,name){
		idx <- which(name==names(x))
		if( length(idx) )
			return(x[,idx])
		else
			stop("Name not in Population names")
	}
)





#' Setting data columns in a \code{Population}
#' 
#' This is an overload for $<- to set entire data columns in a 
#'	\code{Population}
#' 
#' 
#' @name $<-
#' @note This function is the main way to add additional columns of data onto 
#'	an existing \code{Population} object.  Just make sure that the named 
#'	column does not exhist already...
#' @docType methods
#' @aliases $<-,Population-method
#' @rdname Population-accessors
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' summary(pop)
#' pop$elevation <- runif(3)
#' summary(pop)
#'
setReplaceMethod("$",
	signature("Population"),
	function(x,name,value){
		if( length(value) != dim(x)[1] && !is.null(value) )
			stop("Replacement columns must have the same number of rows.")
		
		idx <- which(name==names(x))
		if( length(idx) ) {
			if( is.null(value) ) 
				x@values[idx] <- NULL
			else
				x[,idx] <- value
		}
		else 
			x@values[[name]] <- value

		invisible(x)
	}
)



#' Accessing data column in a Population object by numeric or logical index
#' 
#' This is an overload for [ indexing data columns in a \code{Population} by 
#'	numerical index instead of by name. This allows you to get rows, slices, 
#'	or use other logical operators to get the subset of the \code{Population} 
#'	that you want.
#' 
#' @name [-Population-accessors
#' @aliases [,Population-method
#' @docType methods
#' @rdname Population-accessors
#' @note You can do some interesting slicing with this function.  TODO: add 
#'	more examples.
#' @return It depends upon how you call this one, it can return a whole column 
#'	(all the same) data types if you do something like \code{pop[,1]}, it can 
#' 	return a row (an individual) if your indexes are \code{pop[1,]} or it can 
#'	return an individual element if you use both indexes \code{pop[1,2]}.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>  
#' @examples
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' pop[,1]
#' pop[pop$Pop!="Loreto",]
#'
setMethod("[",
	signature("Population"),
	function(x,i,j,drop){
	
		
		if( missing(j) && missing(i) )
			return(NULL)
			
		else if( (!missing(i) && max(i) > dim(x)[1]) || 
				 (!missing(j) && max(j)>dim(x)[2]) )
			stop("Requested index exceeds size of the population.")
		
		# missing column request-give entire row	
		else if( missing(j) && !missing(i) ){
			ret <- list()
			retPop <- Population()
			for(name in names(x) )
				retPop[,length(names(retPop))+1] <- x@values[[name]][i]
			names(retPop@values) <- names(x)
			for(name in names(retPop@values) ){
				if( is(retPop@values[[name]],"factor"))
					retPop@values[[name]] <- as.factor( as.character( retPop@values[[name]]))
			}
			
			return(retPop)
		}

		# missing row and not col Return as raw list
		else if( missing(i) && !missing(j) ) {
			return( x@values[[j]])
		}
		
		# missing neither, get the whole enchelada
		else if( !missing(i) && !missing(j) ) {
			
			# single element - return as itself
			if( length(i)==1 && length(j)==1 )
				return( x@values[[j]][[i]])
				
			#multiple rows of single column  Return as raw list
			else if( length(i)>1 && length(j)==1 ) {
				return(x@values[[j]][i])
			}
			
			#multiple columns of single row Return as population
			else if( length(i)==1 && length(j)>1) {
				ret <- list()
				keys <- names(x)[j]
				retPop <- Population()
				for(name in keys )
					retPop[,length(names(retPop))+1] <- x@values[[name]][i]
				names(retPop@values) <- keys
				return(retPop)
			}
			
			#multiple rows and columns Return as population
			else if( length(i)>1 && length(j)>1){
				ret <- list()
				keys <- names(x)[j]
				retPop <- Population()
				for(name in keys )
					retPop[,length(names(retPop))+1] <- x@values[[name]][i]
				names(retPop@values) <- keys
				return(retPop)				
			}
		}			
	}
)




#' Setting data columns in a \code{Population} using numeric index
#' 
#' This is an overload for [<- to set entire data columns in a \code{Population} by numeric index
#' 
#' 
#' @name [<--Population-accessors
#' @note This function can replace an element of the \code{Population} (e.g., 
#'	\code{pop[2,3] <- 42}), an entire data column (\code{pop[,1] <- 
#'	rnorm(10)}), or an entire row (\code{ pop[1,] <- newInd}; as long as the 
#' 	\code{newInd} is a list with the same exact names as the pop).  If you to 
#'	replace an element or row that is of a different \code{class}, the 
#'	code will barf and not let you... (unless you put a column as \code{NULL} 
#'	as that is the default way to remove it in \code{data.frames}). 
#' @docType methods
#' @aliases [<-,Population-method
#' @rdname Population-accessors
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' pop[1,1] <- "Loreto"
#' pop
#' pop[,2] <- rep(0,3)
#' pop
#'
setReplaceMethod("[",
	signature("Population"),
	function(x,i,j,value) {
		
		if( missing(j) && missing(i) )
			warning("you cannot replace everything using [] notation...")


		# replace row
		else if( missing(j) && !missing(i) ) {
			
			if( length(value) == length(names(x))){
				if( all(names(value) == names(x@values)) ){
					
					for(j in seq(1:length(value))){
						
						if( is(value,"Population")){
							x[i,j] <- value[1,j]
						}
						# passing values as a population
						else {
							if( is(value[[i]],"list"))
								x[i,j] <- unlist(value[[i]])
						
							else
								x[i,j] <- value[[i]]
						}
					}
					
					
				}
				else
					stop("Replacement row must have Population names")
			}
			else 
				stop("Row replacements must be the right number of columns")
		}
		
		# replace col
		else if( missing(i) && !missing(j) ) {
			if( is.null(value) )
				x@values[[j]] <- NULL
			
			else if( j > length(x) ){
				lbl <- paste("V",length(x)+1,sep="")
				x@values[[lbl]] <- value
			}
			
			else if( length(value) == dim(x)[1] ) {
				x@values[[j]] <- value
								
			}
			else 
				stop("You add data of the same length")
			
		}
		
		# replace cell
		else if( !missing(i) && !missing(j) ) {
			if( length(j)>1) {
				for(idx in j)
					x[i,idx] <- unlist(value[idx])
			}
			else if( class(value) == class(x@values[[j]][[i]])){
				x@values[[j]][[i]] <- value
			}
			else
				stop("Replacements must be the same class")
		}
		x
	}
)





#' Accessing data column in a Population object by bracketted name
#' 
#' This is an overload for [[ indexing data columns in a \code{Population} by 
#'	name or names.  This essentially calls the [,idx] function.
#' 
#' @name [[-Population-accessors
#' @aliases [[,Population-method
#' @docType methods
#' @rdname Population-accessors
#' @note Only returns a single column.
#' @return A list of values for the column
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>  
#' @examples
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' popLabel <- "Pop"
#' pop[[popLabel]]
#'
setMethod("[[",
	signature("Population"),
	function(x,i,...,exact=TRUE){
		if( !(i %in% names(x) ) )
			return(NULL)
		else
			return(x@values[[i]])
	}
)
	
		
	









##############################################################################
#								NEW GENERIC FUNCTIONS                        #
##############################################################################



#' Returns data names for specific kind of class
#' 
#' @param pop A \code{Population} object
#' @param type The \code{class} of data column of interest.
#' @return A list of the names of \code{class(type)} in the \code{Population}
#' @docType methods
#' @aliases column.names
#' @rdname column.names_Population
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
setGeneric("column.names", def=function(pop,type) standardGeneric("column.names") )


#' @rdname column.names_Population
#' @aliases column.names,Population-method
#' @exportMethod column.names
#' @examples \dontrun{
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' column.names(pop,type="character")
#' column.names(pop,type="Locus")
#' column.names(pop,type="factor")
#'
#' }
setMethod("column.names",
	"Population",
	function(pop,type) {
		nCols <- length(names(pop))
		ret <- names(pop)
		for(i in 1:nCols)
			if( class(pop@values[[i]][[1]]) != type )
				ret[i] <- NA
		return(ret[!is.na(ret)])
	}
)






#' Returns a \code{Frequencies} object (or objects)
#' 
#' @param pop A \code{Population} object
#' @param loci A character vector of locus names to use
#' @return A list of the \code{Frequencies} objects keyed by locus name
#' @note If nothing is supplied for \code{loci} then all loci are returned.
#' @docType methods
#' @aliases allele.frequencies
#' @rdname allele.frequencies_Population
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
setGeneric("allele.frequencies",
	def=function(pop,loci) standardGeneric("allele.frequencies") )


#' @rdname allele.frequencies_Population
#' @aliases allele.frequencies,Population-method
#' @exportMethod allele.frequencies
#' @examples \dontrun{
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' freqs <- allele.frequencies(pop,"TPI")
#' freqs
#'
#' }
setMethod("allele.frequencies",
	signature(pop="Population"),
	function(pop,loci) {
		ret <- list()
		if( missing(loci))
			loci <- column.names(pop,"Locus")
		for(locus in loci ){
			ret[[locus]] <- Frequencies( pop@values[[locus]])
		}
		#if( length(ret)==1 ) ret <- ret[[1]]
		return(ret)
	}
)




#' Returns a list of \code{Population} objects
#' 
#' @param pop A \code{Population} object
#' @param stratum A character vector of the stratum to partition on
#' @return A list of the \code{Populations} indexed by \code{stratum} name
#' @docType methods
#' @aliases partition
#' @rdname partition_Population
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>

setGeneric("partition",
		def=function(pop,stratum) standardGeneric("partition") )


#' @rdname partition_Population
#' @aliases partition,Population-method
#' @exportMethod partition
#' @examples \dontrun{
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov <- rnorm(3)
#' pop <- Population( Pop=strata, env=cov, TPI=loci )
#' subpops <- partition(pop,"Pop")
#' print(subpops)
#'
#' }
setMethod("partition",
	"Population",
	function(pop,stratum) {
		ret <- list()
		idx <- which( stratum==names(pop))
		if( length(idx) ) {
			subpops <- unique( pop@values[[stratum]])
			for(name in subpops ){
				ret[[name]] <- pop[ pop[,idx]==name , ]
				ret[[name]][,idx] <- NULL
			}
		}
		return(ret)
	}
)









#' Returns the numeric indices of data column names
#' 
#' @param pop A \code{Population} object
#' @param names A character vector of the stratum 
#' @return A vector of numeric column indices 
#' @docType methods
#' @aliases indices
#' @rdname indices_Population
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>

setGeneric("indices",
		def=function(pop,colNames) standardGeneric("indices") )


#' @rdname indices_Population
#' @aliases indices,Population-method
#' @exportMethod indices
#' @examples \dontrun{
#'
#' loci <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' cov1 <- rnorm(3)
#' cov2 <- rnorm(3)
#' pop <- Population( Pop=strata, env1=cov1, TPI=loci, env2=cov2 )
#' indices( pop, c("env1","env2"))
#'
#' }
setMethod("indices",
	"Population",
	function(pop,colNames) {
		ret <- numeric(0)
		all.names <- names(pop)
		for(name in colNames){
			idx <- which( name==all.names ) 
			if( length(idx))
				ret <- append( ret, idx )
		}
		return(ret)
	}
)







#' Returns multivariate genotypes for all loci
#' 
#' @param pop A \code{Population} object
#' @param loci The loci to use.  If this parameter is missing then
#'	all loci will be used 
#' @param ploidy This is a multiplication factor for the coding of
#'	individual alleles.  The \code{Locus} class method \code{as.vector}
#'	returns counts of alleles whose \code{sum{as.vector(locus)}=ploidy}
#'	but there are times when you do not want to have the presence of 
#'	an allele=1 but rather are interested in the freqeuncy of the allele
#'	at the locus (e.g. should be 0.5 for a diploid individual).  Set
#'	ploidy=1 for multivariate.loci to be counts (the default setting) 
#'	or to the ploidy of the locus (e.g., ploidy=2) for 
#'	frequencies at the locus.
#' @return A multivariate matrix where all genotypes will be 
#'	turned into a single concatinated vector.
#' @docType methods
#' @aliases multivariate.loci
#' @rdname multivariate.loci_Population
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @seealso \code{\link{as.vector}}

setGeneric("multivariate.loci",
		def=function(pop,loci,ploidy) standardGeneric("multivariate.loci") )


#' @rdname multivariate.loci_Population
#' @aliases multivariate.loci,Population-method
#' @exportMethod multivariate.loci
#' @examples \dontrun{
#'
#' loc1 <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' loc2 <- c( Locus(c(5,5)),Locus(c(5,7)),Locus(c(5,7)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' pop <- Population( Pop=strata, TPI=loc1, PGM=loc2 )
#' multivariate.loci(pop)
#'
#' }
setMethod("multivariate.loci",
	"Population",
	function(pop,loci=NULL,ploidy=1) {
		if( missing(loci) || is.null(loci) )
			loci <- column.names(pop,"Locus")
		if( !length(loci))
			stop("You need to have some loci...")
		if(missing(ploidy))
			ploidy<-1
		
		all.freqs <- allele.frequencies( pop, loci ) 
		N <- dim(pop)[1]
		all.alleles <- lapply(all.freqs, names)
		K <- sum(unlist(lapply(all.alleles, length)))
		ret <- matrix(0,nrow=N,ncol=K)
		for(i in 1:N){
			ind <- numeric(0)
			idxs <- indices(pop,loci)
			for(idx in idxs) {
				alleles <- names(all.freqs[[ names(pop)[idx] ]])
				loc <- pop[i,idx]
				if( is.na(loc) )
					vec <- get.frequencies(all.freqs[[names(pop)[idx]]])
				else
					vec <- as.vector(loc,alleles) * (1/ploidy)
				ind <- append( ind, vec )
			} 			
			ret[i,] <- ind
		}
		
		colnames(ret) <- unlist( lapply(loci, 
							function(loc,f) paste(loc,f[[loc]],sep="."), f=all.alleles ))
		
		return(ret)
	}
)


#' Returns tally of genotypes
#' 
#' @param pop A \code{Population} object
#' @param loci The locus to use.
#' @return A numeric vector with names corresponding to genotypes
#' @docType methods
#' @aliases genotype.counts
#' @rdname genotype.counts_Population
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @seealso \code{\link{as.vector}}

setGeneric("genotype.counts",
		def=function(pop,locus) standardGeneric("genotype.counts") )


#' @rdname genotype.counts_Population
#' @aliases genotype.counts,Population-method
#' @exportMethod genotype.counts
#' @examples \dontrun{
#'
#' loc1 <- c( Locus(c(1,2)),Locus(c(2,2)),Locus(c(2,2)) )
#' strata <- c("Cabo","Cabo","Loreto")
#' pop <- Population( Pop=strata, TPI=loc1 )
#' genotype.counts(pop,"TPI")
#'
#' }
setMethod("genotype.counts",
	"Population",
	function(pop,locus=NULL) {
		if( missing(locus) || is.null(locus) )
			stop("You need to indicate which locus to use...")
		if( !(locus %in% column.names(pop,"Locus") ))
			stop("That is not a Locus object...")
		r <- lapply( pop@values[[locus]], 
			 			function(x) {
							f <- paste(sort(unlist(strsplit(as.character(x),":"))),collapse=":")
							return(f)})
		g <- unlist(r)
		g <- g[g!="NA"]
		ret.table <- table(g)
		ret <- as.numeric(ret.table)
		names(ret) <- names(ret.table)
		return(ret)
	}
)








