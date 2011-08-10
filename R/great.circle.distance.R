#' Get the "great circle" distance (in km) between two points
#'
#' Calculates the distance between two points given as latitude and longitude
#'	using the law of inverse sines approach.
#'
#' @name great.circle.distance
#' @param lon1 Decimal longitude of first point
#' @param lat1 Decimal latitude of first point
#' @param lon2 Decimal longitude of the second point
#' @param lat2 Decimal latitude of the second point
#' @return Great circle distance between points in km.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'
great.circle.distance <- function( lon1, lat1, lon2, lat2 ){
	if( missing(lon1) || missing(lat1) || missing(lon2) || missing(lat2) )
		stop("All four parameters are required for this function")
	deg2rad <- function(x) { x * pi/180 }
	lon1 <- deg2rad(lon1)
	lat1 <- deg2rad(lat1)
	lon2 <- deg2rad(lon2)
	lat2 <- deg2rad(lat2) 
	r.earth <- 6371
	dist <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(lon2-lon1)) * r.earth
	dist
}

