#' Get Google Pie Chart URL
#'
#' This function creates a fully formed URL for extracting pie charts
#' 	from the google charts static API.
#'
#' @param x The values to create the pie chart from
#' @param labels The labels to attach to those values
#' @param color The colors to use.
#' @return A URL 
#' @seealso \code{pies.on.map}
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
#'
google.pie.chart <- function( x, color,labels=NULL ) {
	if( missing(x) || !length(x) )
		return("http://www.youtube.com/watch?v=oHg5SJYRHA0")

	ret <- "http://chart.apis.google.com/chart?"
	if( !missing(labels) )
		ret <- paste(ret, "chf=bg,s,00000000&amp;chxs=0,FFFFFFFF,30&amp;chxt=x&amp;chs=250x150&amp;cht=p",sep="")
	else 
		ret <- paste(ret, "chf=bg,s,00000000&amp;chs=75x75&amp;cht=p",sep="")
		
	
	ret <- paste(ret,"&amp;chd=t:",sep="")
	
	# make x integers with smallest 1%
	x <- as.character(round(x/sum(x)*100))
	freqs <- paste(x,collapse=",")
	ret <- paste( ret, freqs, sep="" )
	
	# add colors for each group
	ret <- paste(ret,"&amp;chco=4362FB",sep="")
	
	
	# add labels
	if( !missing(labels) && length(labels)==length(x) ){
		labels <- paste(as.character(labels),collapse="|")
		ret <- paste(ret,"&amp;chl=",labels,sep="")
	}
	
	return(ret)
}
