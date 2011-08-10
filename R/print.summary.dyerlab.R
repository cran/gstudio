##############################################################################
# 							  Print Summary Dyerlab                          #
#                                                                            #
# 						    R.J. Dyer <rjdyer@@vcu.edu>                      #
#                                                                            #
##############################################################################


#' Printing of a summary.Locus object
#'
#' This prints the \code{summary.gstudio} S3 class.  Several of the objects 
#'	found in the \code{gstudio} package are summarized as \code{list} objcts.  
#'	This prints out their content in a regular fashion.
#'
#' @param x A \code{summary.gstudio} object
#' @param ... Ignored for the most part.
#' @rdname print_summary.gstudio
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
print.summary.gstudio <- function(x,...){
	for(name in names(x))
		cat(name,":",x[[name]],"\n")
}



