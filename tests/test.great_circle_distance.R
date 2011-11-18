test.great_circle_distance <- function() {
	d1 <- great.circle.distance(0,0,90,0)
	checkTrue( (d1-10000) < 10 )

	d2 <- great.circle.distance(0,90,0,0)
	checkTrue( (d2-10000) < 10 )
	
	
	
}
