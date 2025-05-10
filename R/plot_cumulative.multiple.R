"plot_cumulative.multiple" <-
function(list.F, labels=NULL, min.x=-1, colors=TRUE) {
  MAX_INTEGRALS=15; 
  minimum=1;
	num.F <- length(list.F);
	if (num.F > 15)
	  num.F <- 15;
	if (min.x == -1) {
	  for (i in 1:num.F) {
	    l <-  cumulative.values (list.F[[i]]);
	    localmin <- min(l$x);
	    if (localmin<minimum)
	      minimum <- localmin;
	  }
	}
	else
	  minimum = min.x;
	if (is.null(labels)) {
	  lab <- 2:(num.F+1);
		lab <- paste("k=",lab,sep="")
	}
	else 
	  lab <- paste("k=",labels,sep="");
	# making line patterns (up  to 15)	
	lpat <- c("81","44", "13", "1343", "73", "2262", "4424", "3113", "2626", "6622", "1242", "6222", "2424", "8828", "6246");
  plot(list.F[1],xlim=c(minimum,1), ylim=c(0,1), xlab="similarities", ylab="cumulative distribution", main="", type="n");
	if (colors == TRUE) {
	  for (i in 1:num.F) {
	    graphics::lines(list.F[[i]],lty=lpat[i], do.points=FALSE, verticals=TRUE, col.hor=i, col.vert=i); 
	    # text(l$x[1],l$y[1],pos=1,labels=lab[i]);
	  }
	  # making legend
	  graphics::legend(minimum,1,legend=lab,lty=lpat[1:num.F], col=1:num.F);
	}
  else {
	  for (i in 1:num.F) {
		  graphics::lines(list.F[[i]],lty=lpat[i], do.points=FALSE, verticals=TRUE); 
	    #l <-  cumulative.values (list.F[[i]]);
	    # lines(l$x,l$y,lty=lpat[i]); 
	  }
	  # making legend
	  graphics::legend(minimum,1,legend=lab,lty=lpat[1:num.F]);	
	}
}

