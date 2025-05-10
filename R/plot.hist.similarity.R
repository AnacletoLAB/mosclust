"plot.hist.similarity" <-
function(sim, nbins=25) {
  num.samples <- length(sim);
	minimum <- min(sim);
	if (minimum> 0.5)
	  minimum <- 0.5;
  graphics::hist(sim, breaks=seq(minimum,1,length=nbins), ylim=c(0,num.samples), main="", xlab="Similarity");
}

