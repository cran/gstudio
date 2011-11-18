##############################################################################
# 							    paternity                            
#                                                                             
# 						    R.J. Dyer <rjdyer@@vcu.edu>                        
#                                                                             
##############################################################################

#' Performs paternity exclusion using Fractional analysis
#'
#' Simple function that estimates basic paternity exclusion for a family array
#'	using fractional paternity from \code{transition.probability}
#'
#' @param pop A population that has all the individuals.
#' @param familyID The IndID for the family (see notes).
#' @param indCol Heading for IndID column in \code{Population} (see note)
#' @param offCol Heading for OffID column in \code{Population} (see note)
#' @return A \code{paternity} object.
#' @note The form of a mixed offspring/parent \code{Population} has to have at 
#'	least the following.  All adults must have a unique ID number that will 
#'	be indexed by the argument indCol (default='IndID').  Offspring for a particular
#'	maternal individual all have the same indCol, that of the mother.  A second
#'	identification column is also required, indexed by offCol (default='OffID'), that
#'	is used to differentiate among half-sibs within a maternal offspring array.  Mothers
#'	must have OffID=0 and all of her offspring will have a non-zero value for this.
#'	This approach allow you to have all your data kept in a single \code{Population}.
#' @seealso \code{offspring.array}, \code{transition.probability}
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#' @examples
#'  
#' data(cornus_florida)
#' f <- paternity( cornus_florida, familyID=474)
#' print(f)
#'
paternity <- function( pop, familyID=NULL, indCol="IndID", offCol="OffID" ) {
	
	if( !is(pop,"Population") )
		stop("Must pass a population to this function.")
	if( is.null(familyID) )
		stop("You must specify a family id for this anlaysis.")
		
	o <- offspring.array(pop,familyID,indCol,offCol)
	if( !length(o$mom) )
		stop("You need to have a mother to do the paternity exclusion.")
	if( !length( o$offspring ) )
		stop("You need to have offspring for that family to do paternity exclusion...")
	
	dads <- pop[ pop[[offCol]]==0, ]

	nDads <- dim(dads)[1]
	nOff <- dim(o$offspring)[1]
	locusCols <- column.class(dads,"Locus")
	
	ret <- list( mom=familyID, numOff=nOff, paternity=list() )
	

	# go through the offspring
	for( i in 1:nOff ){
		off <- o$offspring[i,]
		lambda <- rep(0,nDads)
		names(lambda) <- dads[, which(names(pop)==indCol) ]

		# go through the dads
		for(j in 1:nDads){
			p <- 1.0
			
			# go through the loci
			for(k in locusCols){
				t <- transition.probability(o$mom[1,k], off[1,k], dads[j,k])
				if( t > 0 )
					p <- p * t
				else {
					p <- 0.0
					break;
				}
			}
			lambda[j] <- p
		}
		lambda <- lambda[lambda>0]

		if( length(lambda) ) {
			offKey <- as.character(off[[offCol]])
			frac <- lambda/sum(lambda)
			ret$paternity[[offKey]] <- frac
		}
	}
	
	class(ret) <- "paternity"
	return(ret)
}

#' Print paternity object
#'
#' Overload function for printing out a paternity result for a single family
#' @param x An object of type class(x) = "paternity"
#' @param ... Ignored
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @method print paternity
#' @export
#'
print.paternity <- function( x, ... ){
	ret <- "Paternity Analysis:\n"
	ret <- paste(ret," Family ID:", x$mom,"\n")
	ret <- paste(ret," Number of Offspring:", x$numOff,"\n")
	offs <- x$paternity
	offKeys <- sort(names(offs))
	
	ret <- paste(ret," Offspring Assigned Paternity:",length(offKeys),"\n")
	if( length(offKeys) > 0 ) {
		ret <- paste(ret," Fractional Paternity (off: dad(prob) ):\n")
		for( i in 1:length(offKeys) ) {
			ret <- paste(ret,"    ",offKeys[i],":",sep="")
			f <- offs[[offKeys[i]]]
			n <- names(f)
			for(j in 1:length(f))
				ret <- paste(ret, n[j],"(",f[j],") ",sep="")
			ret <- paste(ret,"\n")
		}	
	}
	cat(ret)
	invisible(x)
}







