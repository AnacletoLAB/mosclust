"plot.cumulative" <-
function(F) {
  plot(F, xlim=c(0,1), ylim=c(0,1), xlab="similarities", ylab="cumulative distribution", main="", 
	     do.points=FALSE, verticals=TRUE);
}

