"plot.pvalues" <-
function(l,alpha=1e-02,legendy=0, leg_label=NULL, colors=TRUE) { 
  n.test <- length(l);
	xlab <- l[[1]]$ordered.clusterings;
	n.clusterings <- length(l[[1]]$ordered.clusterings);
	xlab <- paste("k=",xlab,sep="");
	lpat <- c("81","44", "13", "1343", "73", "2262", "4424", "3113", "2626", "6622", "1242", "6222");
	
	exist_zero <- FALSE;
	x<- numeric(n.test*n.clusterings);
	z <- numeric(0); miny = 1;
	for (i in 1:n.test) {
	  for (j in 1:n.clusterings) {
		  if (l[[i]]$p.value[j] == 0) {
			  z <- c (z,i,j);
				exist_zero <- TRUE;
				next;
			}
			else if (l[[i]]$p.value[j]<miny)
			  miny <- l[[i]]$p.value[j];
		}
	}
	if (exist_zero) {
	  miny <- miny/10;
	  for (i in seq(1,length(z),by=2))
		  l[[z[i]]]$p.value[z[i+1]] <- miny;
	}
	
  plot(1:n.clusterings, l[[1]]$p.value,xlim=c(1,n.clusterings), ylim=c(miny,2), xlab="clusterings", 
	ylab="log p-value", main="", type="n", lty=lpat[1], log="y", axes=FALSE);
	graphics::axis(1, at=1:n.clusterings, labels=xlab);
    graphics::axis(2);
	#if (n.test>1)
	if (colors == TRUE) 
	  for (i in 1:n.test) {
	      graphics::lines(1:n.clusterings,l[[i]]$p.value,lty=lpat[i], type="o", col=i) 
	    # text(l$x[1],l$y[1],pos=1,labels=lab[i]);
		}
	else 
	  for (i in 1:n.test) {
	      graphics::lines(1:n.clusterings,l[[i]]$p.value,lty=lpat[i], type="o"); 
	    # text(l$x[1],l$y[1],pos=1,labels=lab[i]);
	  }	 
    graphics::lines(1:n.clusterings,rep(alpha,n.clusterings),lty=2, type="l");
    graphics::text(ceiling(n.clusterings/2),alpha,labels=paste("alpha=", alpha));
	# making legend
	if (is.null(leg_label))
	   leg_label <- paste("test ",1:n.test,sep="");
	if (legendy!=0)
	  if (colors == TRUE)
	      graphics::legend(1,legendy, legend=leg_label,lty=lpat[1:n.test], col=1:n.test)
		else
		  graphics::legend(1,legendy, legend=leg_label,lty=lpat[1:n.test]);
}

