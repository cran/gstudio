##############################################################################
# 							    Read Population                              
#                                                                             
# 						    R.J. Dyer <rjdyer@@vcu.edu>                        
#                                                                             
##############################################################################

#' Loads a population from a *.csv file
#'
#' General function to load a population from a file.
#'
#' @param file The path to the data file, this can be optional if you are 
#'	loading a file from a google spreadsheet (see \code{googleURL} below).
#' @param googleURL The URL to the shared spreadsheet 
#' @param num.single.digit The number of loci that are represented by a single 
#'	characer.  These will be loaded as the right most \code{num.single.digit} 
#'	number of columns of data.  This is for haploid, dominant, and signle 
#'	digit snp encoding (see also \code{Locus} object for snp encoding).
#' @param as.snp.minor A flag to change SNP minor allele encoding 0/1/2 to 
#'	normal "A" and "B" alleles.
#' @param header A logical value indicating that the first line is column 
#'	labels (defaults to TRUE).
#' @param sep The character that separates columns in your data set 
#'	(default=",")
#' @param split This is an option (default=NULL) to split the indicated columns
#'	of genotypes into alleles.  This will take a genotype 33 and split it into 
#'	\code{c(3,3)} for construction of the loci.  This is so that you don't have
#'	to do any odd scripting stuff to get your old genotype files into R (n.b., 
#'	it will split in half for diploid, so 0101 will be turned into 
#'	\code{ c("01","01") } ).  To use this, list the column numbers to split.
#' @return A fully formed \code{Population}
#' @note The \code{numLoci} option can be optional IF the loci are denoted 
#'	with a colon ':' (e.g., not haploid or binary).  If you do not have 
#'	markers of ploidy or greater, then you need to put the number of genetic 
#'	loci here AND have the the loci as the right-most \code{numLoci} columns 
#'	of the data file.  This means that NONE of your non-genetic columns can 
#'	have a colon in them...
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
read.population <- function( 	file=NULL, 
								googleURL=NULL,
								num.single.digit=0,
								as.snp.minor=FALSE, 
								header=TRUE, 
								sep=",",
								split=NULL) {
	if( !is.null(googleURL) ){
		require(RCurl)
		raw <- getURLContent( googleURL )
		if( "Content-Type" %in% names(attributes(raw))){
			if( "text/plain" == attr(raw,"Content-Type")[1] & length(raw) ) {
				con <- textConnection(raw[1])
				df <- read.table(con,sep=sep,header=header)
				close(con)
			}
		}
		else {
			stop("Malformed googleURL")
		}
		connection <- getURL(googleURL)
		
	}
	else if( !is.null(file)){
		df <- read.table(file=file,header=header,sep=sep)
	}
	else {
		stop("File was null")
	}
	
	thePop <- Population()
	nCols <- dim(df)[2]
	labels <- names(df)
	cat("Loading Data Columns: ")
	for( i in 1:nCols ){
		data <- df[,i]
		cat(".",sep="")
		if( !missing(split) && !is.null(split) && (i %in% split) ) {
			data <- as.character(data)
			cat("Column",i,"is being split for old style locus\n")
			splitGeno <- function(geno) {
				if( is.na(geno) ) return(Locus())
				nc <- nchar(geno)/2
				return( Locus( c(substring(geno,1,nc),substring(geno,nc+1) ), ) )
			}
			thePop[,i] <- unlist( lapply(data, function(x) splitGeno(x) ))
		}
		else if( !is(data,"factor") )
			thePop[,i] <- data
		else {
			
			# a haploid column of data
			if( num.single.digit && i >= (nCols-num.single.digit) )
				thePop[,i] <- unlist(lapply(as.character(data), function(x) Locus(x,as.snp.minor=as.snp.minor)))
			else {
				# levels have something with a colon in it
				alleles <- unlist(lapply(as.character(data), 
									function(x) strsplit(x,":")),recursive=F)
				if( any(unlist(lapply(alleles,length))>1) )
					thePop[,i] <- lapply(alleles,Locus)
				
				else
					thePop[,i] <- data
			}
			
		}
		cat("\n")
	}
	names(thePop@values) <- labels

	return(thePop)
}
