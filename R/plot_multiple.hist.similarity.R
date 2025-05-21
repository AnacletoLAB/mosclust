"plot_multiple.hist.similarity" <-
function(S, n.col=3, labels=NULL, nbins=25) {
  num.samples <- ncol(S);
	n.graph <- nrow(S);
	n.row <- ceiling(n.graph/n.col);
	oldpar <- graphics::par(no.readonly = TRUE) 
	on.exit(graphics::par(oldpar))
	op <- graphics::par(mfrow=c(n.row,n.col));
	minimum <- min(S);
	if (minimum> 0.5)
	  minimum <- 0.5;
	if (is.null(labels))
	  lab <- 2:(n.graph+1)
	else
	  lab <- labels;
	for (i in 1:n.graph) {
	  title <- paste("k = ", lab[i]);
    graphics::hist(S[i,], breaks=seq(minimum,1,length=nbins), ylim=c(0,num.samples), main="", xlab=title, ylab="");
	}
	graphics::par(op);
}

