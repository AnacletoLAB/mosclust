"plot_cumulative" <-
function(Fun) {
  plot(Fun, xlim=c(0,1), ylim=c(0,1), xlab="similarities", ylab="cumulative distribution", main="", 
	     do.points=FALSE, verticals=TRUE);
}

