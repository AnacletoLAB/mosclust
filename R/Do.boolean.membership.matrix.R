"Do.boolean.membership.matrix" <-
function(cl, dim.M, examplelabels) {
  M <- matrix(integer(dim.M*dim.M), nrow=dim.M);
	colnames(M) <- rownames(M) <- examplelabels;
	singletons <- integer(dim.M);  
		 c <- length(cl); # number of clusters 
	   for (j in 1:c) {
		   n.ex <- length(cl[[j]]);
			if (n.ex == 1)
			  singletons[cl[[j]][1]] <- 1
			else {
			    for (x1 in 1:(n.ex-1)) {
			       for (x2 in (x1+1):n.ex) {
			         x <- cl[[j]][x1];
				       y <- cl[[j]][x2];
			         M[x,y] <- 1;
			       }
			    }
			 }
		 }
	for (x1 in 1:(dim.M-1)) 
		for (x2 in (x1+1):dim.M) 
		  M[x2,x1] <- M[x1,x2];
	for (x in 1:(dim.M)) 
	   M[x,x] <- singletons[x];
	return(M);
}

